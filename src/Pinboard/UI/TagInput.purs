module Pinboard.UI.TagInput
  ( Input
  , State
  , Query(..)
  , Output(..)
  , Config
  , HTML
  , component
  ) where

import Prelude
import Data.Array               (find, null, snoc, init, mapWithIndex, (!!), deleteAt, length)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype, wrap, unwrap)
import Data.Foldable            (elem)
import Data.Tuple               (Tuple(..), fst, snd)
import Data.Filterable          (maybeBool)
import Data.Time.Duration       (Milliseconds)

import Control.Monad.Aff.AVar   (AVAR)
import Control.Monad.Aff.Class  (class MonadAff)

import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP

import DOM                      (DOM)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import DOM.Event.FocusEvent     (FocusEvent)
import DOM.Event.KeyboardEvent  (KeyboardEvent, key, metaKey, shiftKey, altKey)

import Pinboard.UI.Debounce     (Debouncer)
import Pinboard.UI.Debounce     as D

-------------------------------------------------------------------------------

type Input = Unit

type Config i m =
  { suggest   :: Array i -> String -> m (Array i)
  , parse     :: String -> i
  , render    :: i -> HTML
  , hideDelay :: Milliseconds
  , showDelay :: Milliseconds }

newtype State i e = State
  { buffer  :: String
  , chosen  :: Array i
  , options :: Options i e }

newtype Options i e = Options
  { visible     :: Boolean
  , options     :: Array i
  , hoverIdx    :: Maybe Int
  , waitToHide  :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | e))
  , waitToShow  :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | e)) }

derive instance newtypeState :: Newtype (State i e) _
derive instance newtypeOptions :: Newtype (Options i e) _

data Query k
  = Init k
  | Exit k
  | OnKey KeyboardEvent k
  | OnBlur FocusEvent k
  | OnInput String k
  | OnFocus FocusEvent k
  | Reject Int k
  | Choose Int k

data Output i
  = OnChosen (Array i)

type HTML      = H.ComponentHTML Query
type DSL i m e = H.ComponentDSL (State i e) Query (Output i) m

-------------------------------------------------------------------------------

component
  :: forall i m e
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => Eq i
  => Config i m
  -> H.Component HH.HTML Query Input (Output i) m
component cfg =
  H.lifecycleComponent
  { initialState
  , render
  , eval
  , receiver
  , initializer:  Just (H.action Init)  -- Maybe (f Unit)
  , finalizer:    Just (H.action Exit) }-- Maybe (f Unit)
  where
    initialState :: Input -> State i e
    initialState _ =
      State
      { buffer:   ""
      , chosen:   []
      , options:  Options
                  { visible:    false
                  , options:    []
                  , hoverIdx:   Nothing
                  , waitToHide: Nothing
                  , waitToShow: Nothing } }

    render :: State i e -> HTML
    render (State s) =
      HH.div
        [ HP.class_ (H.ClassName "tags") ]
        [ renderChosen s.chosen
        , HH.div_
            [ HH.input
                [ HP.placeholder ""
                , HP.type_ HP.InputText
                , HP.value s.buffer
                , HP.autofocus true
                , HE.onBlur (HE.input OnBlur)
                , HE.onFocus (HE.input OnFocus)
                , HE.onKeyDown (HE.input OnKey)
                , HE.onValueInput (HE.input OnInput) ] ]
        , renderOptions s.options ]
      where
        renderChosen :: Array i -> HTML
        renderChosen [] = HH.text ""
        renderChosen xs =
          HH.ul [class_ "selected"] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Reject n)) ]
              [ cfg.render x ]

        renderOptions :: Options i e -> HTML
        renderOptions (Options { visible, options, hoverIdx })
          | not visible   = HH.text ""
          | null options  = HH.text ""
          | otherwise     =
            HH.ul [class_ "suggested"] $ flip mapWithIndex options \n x ->
              HH.li
                [ HE.onClick (HE.input_ (Choose n))
                , class_ (if Just n == hoverIdx then "highlighted" else "") ]
                [ cfg.render x ]

    eval :: Query ~> DSL i m e
    eval q = case q of
      Init k -> pure k
      Exit k -> pure k

      Reject n k -> k <$
        H.modify (rejectChosen n)

      Choose n k -> k <$ do
        H.modify (chooseOption n)

      OnInput v k -> k <$ do
        -- wait a tick for user to stop typing before computing suggestions
        o <- H.gets (unwrap <<< _.options <<< unwrap)
        w <- case o.waitToShow of
                  Nothing -> D.create   cfg.showDelay
                  Just _w -> D.reset _w cfg.showDelay

        H.modify (state   (_ { buffer     = v }) <<<
                  options (_ { waitToShow = Just w }))

        H.fork $ D.whenQuiet w do
          s <- H.gets unwrap
          o <- H.lift (cfg.suggest s.chosen s.buffer)
          H.modify (updateOptions o <<<
                    options (_ { visible = true, waitToShow = Nothing }))

      OnBlur e k -> k <$ do
        -- wait a tick for user to stop clicking before hiding suggestions
        o <- H.gets (unwrap <<< _.options <<< unwrap)
        w <- case o.waitToHide of
                  Nothing -> D.create   cfg.hideDelay
                  Just _w -> D.reset _w cfg.hideDelay

        H.modify (options (_ { waitToHide = Just w }))
        H.fork $ D.whenQuiet w do
          H.modify (chooseBuffer cfg.parse <<<
                    options (_ { visible = false, options = [], waitToHide = Nothing }))

      OnFocus e k -> k <$ do
        -- if we were about to hide suggestions, don't
        o <- H.gets (unwrap <<< _.options <<< unwrap)
        _ <- case o.waitToHide of
                  Nothing -> pure unit
                  Just w  -> D.cancel w
        H.modify (options (_ { waitToHide = Nothing }))

      OnKey e k -> k <$ do
        s <- H.get
        o <- H.gets (unwrap <<< _.options <<< unwrap)

        case key e of
             "Backspace"
               | metaKey e -> H.modify resetState
               | otherwise -> when (bufferIsBlank s) $
                                H.modify rejectLast

             "Enter" -> do
               case o.hoverIdx of
                    Just n -> do
                      noBubble e
                      H.modify (chooseOption n)
                    Nothing
                      | bufferIsBlank s -> pure unit
                      | otherwise       -> do
                        noBubble e
                        H.modify (chooseBuffer cfg.parse)

             x | chooseKey x -> do
               noBubble e
               H.modify (chooseBuffer cfg.parse)
               H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

             "Escape" -> do
               noBubble e
               H.modify clearBuffer

             "Tab"
               | shiftKey e -> noBubble e *> H.modify highlightPrev
               | otherwise  -> noBubble e *> H.modify highlightNext

             "ArrowLeft"
               | not shiftKey e &&
                 not metaKey e &&
                 not altKey e -> do
                   noBubble e
                   H.modify highlightPrev

             "ArrowRight"
               | not shiftKey e &&
                 not metaKey e &&
                 not altKey e -> do
                   noBubble e
                   H.modify highlightNext

             "ArrowDown"
               | not o.visible -> H.modify (options (_ { visible = true }))

             _ -> pure unit

        where chooseKey = flip elem [",", ";", " "]

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

-------------------------------------------------------------------------------

noBubble
  :: forall i e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => KeyboardEvent
  -> DSL i m e Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.keyboardEventToEvent


state :: forall i e a. Newtype (State i e) a => (a -> a) -> (State i e -> State i e)
state f = wrap <<< f <<< unwrap

options :: forall i e a. Newtype (Options i e) a => (a -> a) -> (State i e -> State i e)
options f (State s) = State (s { options = op s.options })
  where op = wrap <<< f <<< unwrap

rejectChosen :: forall i e. Int -> State i e -> State i e
rejectChosen k (State s) = State (s { chosen = fromMaybe [] (deleteAt k s.chosen) })

chooseOption :: forall i e. Int -> State i e -> State i e
chooseOption k (State s) = State (op s.options)
  where
    op (Options o) =
      case o.options !! k of
           Nothing -> s
           Just x  -> s { chosen  = s.chosen `snoc` x
                        , buffer  = ""
                        , options = Options (o
                                    { visible  = false
                                    , hoverIdx = Nothing })}

chooseBuffer :: forall i e. (String -> i) -> State i e -> State i e
chooseBuffer f (State s)
  | s.buffer == "" = State s
  | otherwise      = State (op s.options)
  where
    op (Options o) = s { chosen  = s.chosen `snoc` f s.buffer
                       , buffer  = ""
                       , options = Options (o
                                   { visible  = false
                                   , options  = []
                                   , hoverIdx = Nothing })}

updateOptions :: forall i e. Eq i => Array i -> State i e -> State i e
updateOptions newOptions = options op
  where
    op o =
      case o.hoverIdx of
           Nothing -> o { options = newOptions }
           Just n_ -> let n = findIndex newOptions =<< o.options !! n_
                       in o { options = newOptions, hoverIdx = n }

clearBuffer :: forall i e. State i e -> State i e
clearBuffer (State s) = State (s { buffer  = ""
                                 , options = Options
                                             { visible    : false
                                             , options    : []
                                             , hoverIdx   : Nothing
                                             , waitToHide : Nothing
                                             , waitToShow : Nothing }})

resetState :: forall i e. State i e -> State i e
resetState _ = State { buffer  : ""
                     , chosen  : []
                     , options : Options
                                 { visible     : false
                                 , options     : []
                                 , hoverIdx    : Nothing
                                 , waitToHide  : Nothing
                                 , waitToShow  : Nothing }}

bufferIsBlank :: forall i e. State i e -> Boolean
bufferIsBlank (State s) = s.buffer == ""

rejectLast :: forall i e. State i e -> State i e
rejectLast (State s) = State (s { chosen = fromMaybe s.chosen (init s.chosen) })

highlightPrev :: forall e x. State e x -> State e x
highlightPrev = options op
  where
    op o
      | not o.visible = o
      | otherwise     =
        case o.hoverIdx of
             Nothing -> o { hoverIdx = maybeBool (_ >= 0) (length o.options - 1) }
             Just n  -> o { hoverIdx = maybeBool (_ >= 0) (n - 1) }

highlightNext :: forall e x. State e x -> State e x
highlightNext = options op
  where
    op o
      | not o.visible = o
      | otherwise     =
        case o.hoverIdx of
             Nothing -> o { hoverIdx = maybeBool (_ < length o.options) 0 }
             Just n  -> o { hoverIdx = maybeBool (_ < length o.options) (n + 1) }

findIndex :: forall a. Eq a => Array a -> a -> Maybe Int
findIndex xs x = fst <$> find ((x == _) <<< snd) (mapWithIndex Tuple xs)

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ s = HP.class_ (H.ClassName s)

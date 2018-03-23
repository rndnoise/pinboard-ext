module Pinboard.UI.Component.TagInput
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
import DOM.HTML.HTMLElement     (setClassName)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import DOM.Event.KeyboardEvent  (key, metaKey, shiftKey, altKey)

import Pinboard.UI.Component.Debounce (Debouncer)
import Pinboard.UI.Component.Debounce as D

-------------------------------------------------------------------------------

type Input = Unit

type Config i m =
  { suggest      :: Array i -> String -> m (Array i)
    -- ^ Computes suggestions given the previously
    --   chosen values and the current text buffer

  , parse        :: String -> i
    -- ^ Converts the text buffer to an item

  , renderChoice :: i -> HTML i
    -- ^ Renders a chosen item

  , renderOption :: i -> HTML i
    -- ^ Renders a suggested item

  , textValue    :: i -> String
    -- ^ Convert an item to serializable value

  , hideDelay    :: Milliseconds
    -- ^ Wait after losing focus to hide the suggested items

  , showDelay    :: Milliseconds }
    -- ^ Wait after text entry before computing suggestions

newtype State i eff = State
  { buffer  :: String
    -- ^ The value of the text input field

  , chosen  :: Array i
    -- ^ The chosen items

  , options :: Options i eff }
    -- ^ State of the current suggestions

newtype Options i eff = Options
  { visible     :: Boolean
    -- ^ Whether the suggestions are displayed

  , options     :: Array i
    -- ^ Suggestions computed from Config.suggest

  , hoverIdx    :: Maybe Int
    -- ^ Which suggestion is currently highlighted

  , waitToHide  :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | eff))
  , waitToShow  :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | eff)) }

derive instance newtypeState :: Newtype (State i eff) _
derive instance newtypeOptions :: Newtype (Options i eff) _

data Query i k
  = OnKey ET.KeyboardEvent k
    -- ^ key was pressed in the text buffer

  | OnBlur ET.FocusEvent k
    -- ^ the text entry lost focus

  | OnInput String k
    -- ^ the text buffer changed

  | OnFocus ET.FocusEvent k
    -- ^ the text entry gained focus

  | Reject Int k
    -- ^ a chosen item is deleted

  | Choose Int k
    -- ^ a suggested item is chosen

  | SetChosen (Array i) k
    -- ^ allows a parent component to change the value

data Output i
  = OnChosen (Array i)
    -- ^ contains the now-current set of chosen items

type HTML i      = H.ComponentHTML (Query i)
type DSL i m eff = H.ComponentDSL (State i eff) (Query i) (Output i) m

-------------------------------------------------------------------------------

component
  :: forall i m eff
   . MonadAff (dom :: DOM, avar :: AVAR | eff) m
  => Eq i
  => Config i m
  -> H.Component HH.HTML (Query i) Input (Output i) m
component cfg =
  H.component
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input -> State i eff
    initialState xs =
      State
      { buffer:   ""
      , chosen:   []
      , options:  Options
                  { visible:    false
                  , options:    []
                  , hoverIdx:   Nothing
                  , waitToHide: Nothing
                  , waitToShow: Nothing }}

    render :: State i eff -> HTML i
    render (State s) =
      HH.div
        [ HP.id_ "tags", HP.ref (H.RefLabel "tags") ]
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
        renderChosen :: Array i -> HTML i
        renderChosen [] = HH.text ""
        renderChosen xs =
          HH.ul [class_ "selected"] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Reject n)) ]
              [ cfg.renderChoice x ]

        renderOptions :: Options i eff -> HTML i
        renderOptions (Options { visible, options, hoverIdx })
          | not visible   = HH.text ""
          | null options  = HH.text ""
          | otherwise     =
            HH.ul [class_ "suggested"] $ flip mapWithIndex options \n x ->
              HH.li
                [ HE.onClick (HE.input_ (Choose n))
                , class_ (if Just n == hoverIdx then "highlighted" else "") ]
                [ cfg.renderOption x ]

    eval :: Query i ~> DSL i m eff
    eval q = case q of
      Reject n k -> k <$ do
        H.modify (rejectChosen n)
        H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

      Choose n k -> k <$ do
        H.modify (chooseOption n)
        H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

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
        H.getHTMLElementRef (H.RefLabel "tags") >>= case _ of
          Nothing -> pure unit
          Just el -> H.liftEff (setClassName "" el)

        -- wait a tick for user to stop clicking before hiding suggestions
        o <- H.gets (unwrap <<< _.options <<< unwrap)
        w <- case o.waitToHide of
                  Nothing -> D.create   cfg.hideDelay
                  Just _w -> D.reset _w cfg.hideDelay

        H.modify (options (_ { waitToHide = Just w }))
        H.fork $ D.whenQuiet w do
          H.modify (chooseBuffer cfg.parse <<<
                    options (_ { visible = false, options = [], waitToHide = Nothing }))
          H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

      OnFocus e k -> k <$ do
        H.getHTMLElementRef (H.RefLabel "tags") >>= case _ of
          Nothing -> pure unit
          Just el -> H.liftEff (setClassName "focus" el)

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
               | otherwise -> when (bufferIsBlank s) $ do
                                H.modify rejectLast
                                H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

             "Enter" -> do
               case o.hoverIdx of
                    Just n -> do
                      noBubble e
                      H.modify (chooseOption n)
                      H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)
                    Nothing
                      | bufferIsBlank s -> pure unit
                      | otherwise       -> do
                        noBubble e
                        H.modify (chooseBuffer cfg.parse)
                        H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

             x | chooseKey x -> do
               noBubble e
               H.modify (chooseBuffer cfg.parse)
               H.raise =<< H.gets (OnChosen <<< _.chosen <<< unwrap)

             "Escape" -> do
               noBubble e
               H.modify clearBuffer

             "Tab"
               | not (bufferIsBlank s) -> do
                   noBubble e
                   if shiftKey e
                     then H.modify highlightPrev
                     else H.modify highlightNext

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

      SetChosen xs k -> k <$ do
        H.modify (state (_ { chosen = xs }))

      where chooseKey = flip elem [",", ";", " "]

-------------------------------------------------------------------------------

noBubble
  :: forall i eff m
   . MonadAff (dom :: DOM, avar :: AVAR | eff) m
  => ET.KeyboardEvent
  -> DSL i m eff Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.keyboardEventToEvent


state :: forall i eff a. Newtype (State i eff) a => (a -> a) -> (State i eff -> State i eff)
state f = wrap <<< f <<< unwrap


options :: forall i eff a. Newtype (Options i eff) a => (a -> a) -> (State i eff -> State i eff)
options f (State s) = State (s { options = op s.options })
  where op = wrap <<< f <<< unwrap


rejectChosen :: forall i eff. Int -> State i eff -> State i eff
rejectChosen k (State s) = State (s { chosen = fromMaybe [] (deleteAt k s.chosen) })


chooseOption :: forall i eff. Int -> State i eff -> State i eff
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


chooseBuffer :: forall i eff. (String -> i) -> State i eff -> State i eff
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


updateOptions :: forall i eff. Eq i => Array i -> State i eff -> State i eff
updateOptions newOptions = options op
  where
    op o =
      case o.hoverIdx of
           Nothing -> o { options = newOptions }
           Just n_ -> let n = findIndex newOptions =<< o.options !! n_
                       in o { options = newOptions, hoverIdx = n }


clearBuffer :: forall i eff. State i eff -> State i eff
clearBuffer (State s) = State (s { buffer  = ""
                                 , options = Options
                                             { visible    : false
                                             , options    : []
                                             , hoverIdx   : Nothing
                                             , waitToHide : Nothing
                                             , waitToShow : Nothing }})


resetState :: forall i eff. State i eff -> State i eff
resetState _ = State { buffer  : ""
                     , chosen  : []
                     , options : Options
                                 { visible     : false
                                 , options     : []
                                 , hoverIdx    : Nothing
                                 , waitToHide  : Nothing
                                 , waitToShow  : Nothing }}


bufferIsBlank :: forall i eff. State i eff -> Boolean
bufferIsBlank (State s) = s.buffer == ""


rejectLast :: forall i eff. State i eff -> State i eff
rejectLast (State s) = State (s { chosen = fromMaybe s.chosen (init s.chosen) })


highlightPrev :: forall eff x. State eff x -> State eff x
highlightPrev = options op
  where
    op o
      | not o.visible = o
      | otherwise     =
        case o.hoverIdx of
             Nothing -> o { hoverIdx = maybeBool (_ >= 0) (length o.options - 1) }
             Just n  -> o { hoverIdx = maybeBool (_ >= 0) (n - 1) }


highlightNext :: forall eff x. State eff x -> State eff x
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

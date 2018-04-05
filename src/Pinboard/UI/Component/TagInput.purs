module Pinboard.UI.Component.TagInput
  ( Input
  , State
  , Options
  , Query(..)
  , Output(..)
  , Config
  , HTML
  , component
  ) where

import Prelude
import Control.Monad.Aff.AVar         (AVAR)
import Control.Monad.Aff.Class        (class MonadAff)
import Control.MonadPlus              (guard)
import DOM                            (DOM)
import DOM.Event.Event                (preventDefault)
import DOM.Event.KeyboardEvent        (key, metaKey, shiftKey, altKey)
import DOM.Event.Types                (KeyboardEvent, FocusEvent, keyboardEventToEvent)
import Data.Array                     (find, null, snoc, init, mapWithIndex, (!!), deleteAt, length)
import Data.Filterable                (maybeBool)
import Data.Foldable                  (elem)
import Data.Maybe                     (Maybe(..), fromMaybe)
import Data.Time.Duration             (Milliseconds)
import Data.Tuple                     (Tuple(..), fst, snd)
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Pinboard.UI.Component.Debounce (Debouncer)
import Pinboard.UI.Component.Debounce as D
import Pinboard.UI.Internal.HTML      as PH

-------------------------------------------------------------------------------

type Input i m = Tuple (Config i m) (Array i)

type Config i m =
  { suggest      :: Array i -> String -> m (Array i)
    -- ^ Computes suggestions given the previously
    --   chosen values and the current text buffer

  , parse        :: String -> i
    -- ^ Converts the text buffer to an item

  , renderChoice :: i -> HTML i m
    -- ^ Renders a chosen item

  , renderOption :: i -> HTML i m
    -- ^ Renders a suggested item

  , textValue    :: i -> String
    -- ^ Convert an item to serializable value

  , hideDelay    :: Milliseconds
    -- ^ Wait after losing focus to hide the suggested items

  , showDelay    :: Milliseconds }
    -- ^ Wait after text entry before computing suggestions

type State i m eff = -- TODO: don't care about eff
  { buffer  :: String
    -- ^ The value of the text input field

  , focused :: Boolean
    -- ^ Whether the input field has keyboard focus

  , chosen  :: Array i
    -- ^ The chosen items

  , options :: Options i eff
    -- ^ State of the current suggestions

  , config  :: Config i m }

type Options i eff = -- TODO: don't care about eff
  { visible     :: Boolean
    -- ^ Whether the suggestions are displayed

  , options     :: Array i
    -- ^ Suggestions computed from Config.suggest

  , hoverIdx    :: Maybe Int
    -- ^ Which suggestion is currently highlighted

  , waitToHide  :: Maybe (Debouncer (dom :: DOM | eff))

  , waitToShow  :: Maybe (Debouncer (dom :: DOM | eff)) }

data Query i m k
  = Recv (Input i m) k
    -- ^ The parent component provides an updated `Input`; could have
    --   different selected items or a new `suggest` function due to
    --   updated dictionary of known words.

  | OnKey KeyboardEvent k
    -- ^ Key was pressed in the text buffer

  | OnBlur FocusEvent k
    -- ^ The text entry lost focus

  | DoBlur k
    -- ^ Wait has expired, act on blur event

  | OnInput String k
    -- ^ The text buffer changed

  | DoInput k
    -- ^ Wait has expired, act on input event

  | OnFocus FocusEvent k
    -- ^ The text entry gained focus

  | Reject Int k
    -- ^ A chosen item is deleted

  | Choose Int k
    -- ^ A suggested item is chosen

type Output i = Array i
  -- ^ Contains the now-current set of chosen items

type HTML i m    = H.ComponentHTML (Query i m)
type DSL i m eff = H.ComponentDSL (State i m eff) (Query i m) (Output i) m

-------------------------------------------------------------------------------

component
  :: forall i m eff
   . MonadAff (dom :: DOM, avar :: AVAR | eff) m
  => Eq i
  => H.Component HH.HTML (Query i m) (Input i m) (Output i) m
component =
  H.component
  { initialState
  , render
  , eval
  , receiver: \i -> Just (Recv i unit) }
  where
    initialState :: Input i m -> State i m eff
    initialState (Tuple config chosen) =
      { chosen
      , config
      , buffer:  ""
      , focused: false
      , options: { visible:    false
                 , options:    []
                 , hoverIdx:   Nothing
                 , waitToHide: Nothing
                 , waitToShow: Nothing }}

    render :: State i m eff -> HTML i m
    render s =
      HH.div
      [ PH.classes (join [ pure "tags"
                         , pure "icon-tag"
                         , "focus" <$ guard s.focused]) ]
      [ case s.chosen of
          [] -> HH.text ""
          xs -> HH.ul [ PH.class_ "selected" ]
                $ flip mapWithIndex xs \n x ->
                  HH.li
                  [ HE.onClick (HE.input_ (Reject n)) ]
                  [ s.config.renderChoice x ]

      , HH.div_
        [ HH.input
          [ HP.placeholder case s.chosen of
              [] -> "Comma-separated list of tags"
              _  -> ""
          , HP.type_ HP.InputText
          , HP.value s.buffer
          , HP.autofocus true
          , HP.autocomplete false
          , HP.spellcheck false
          , HE.onBlur (HE.input OnBlur)
          , HE.onFocus (HE.input OnFocus)
          , HE.onKeyDown (HE.input OnKey)
          , HE.onValueInput (HE.input OnInput)
          ]
        ]

      , let o = s.options
         in if not o.visible || null o.options
           then HH.text ""
           else HH.ul [ PH.class_ "suggested" ]
                $ flip mapWithIndex o.options \n x ->
                  HH.li
                    (join
                      [ pure (HE.onClick (HE.input_ (Choose n)))
                      , PH.class_ "highlighted" <$ guard (Just n == o.hoverIdx)
                      ])
                    [ s.config.renderOption x ]
      ]

    eval :: Query i m ~> DSL i m eff
    eval q = case q of
      Recv (Tuple c x) k -> k <$ do
        H.modify (_{ chosen = x, config = c })

      Reject n k -> k <$ do
        H.modify (rejectChosen n)
        H.raise =<< H.gets _.chosen

      Choose n k -> k <$ do
        H.modify (chooseOption n)
        H.raise =<< H.gets _.chosen

      OnInput v k -> k <$ do
        -- Wait a tick for user to stop typing before computing suggestions
        s <- H.get
        w <- case s.options.waitToShow of
               Nothing -> D.create   s.config.showDelay
               Just _w -> D.reset _w s.config.showDelay

        H.modify (_{ buffer     = v      } <<<
         options (_{ waitToShow = Just w }))

        -- This wait prevents issuing rapid-fire requests to complete the
        -- current value (which might not finish before the next keypress)
        H.fork $ D.whenQuiet w do
          eval (DoInput k)

      DoInput k -> k <$ do
        s <- H.get
        o <- H.lift (s.config.suggest s.chosen s.buffer)
        H.modify (updateSuggestions o <<<
         options (_{ visible = true, waitToShow = Nothing }))

      OnBlur e k -> k <$ do
        -- Wait a tick for user to stop clicking before hiding suggestions
        s <- H.get
        w <- case s.options.waitToHide of
               Nothing -> D.create   s.config.hideDelay
               Just _w -> D.reset _w s.config.hideDelay

        H.modify (_{ focused    = false  } <<<
         options (_{ waitToHide = Just w }))

        -- If user clicked a suggestion (`Choose n`), this delay ensures that the
        -- half-entered buffer text isn't chosen instead of the clicked suggestion
        H.fork $ D.whenQuiet w do
          eval (DoBlur k)

      DoBlur k -> k <$ do
        H.modify (chooseBuffer <<<
         options (_{ visible    = false
                   , options    = []
                   , waitToHide = Nothing }))
        H.raise =<< H.gets _.chosen

      OnFocus e k -> k <$ do
        H.modify (_{ focused = true })

        -- If we were about to hide suggestions, don't
        o <- H.gets _.options
        _ <- case o.waitToHide of
               Nothing -> pure unit
               Just w  -> D.cancel w

        H.modify (options (_{ waitToHide = Nothing }))

      OnKey e k -> k <$ do
        s <- H.get
        case key e of
          "Backspace"
            | metaKey e -> H.modify resetState
            | otherwise -> when (bufferIsBlank s) $ do
                             H.modify rejectLast
                             H.raise =<< H.gets _.chosen

          "Enter" -> do
            case s.options.hoverIdx of
                 Just n -> do
                   noBubble e
                   H.modify (chooseOption n)
                   H.raise =<< H.gets _.chosen
                 Nothing
                   | bufferIsBlank s -> pure unit
                   | otherwise       -> do
                     noBubble e
                     H.modify chooseBuffer
                     H.raise =<< H.gets _.chosen

          x | chooseKey x -> do
            noBubble e
            H.modify chooseBuffer
            H.raise =<< H.gets _.chosen

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
            | not s.options.visible ->
                H.modify (options (_{ visible = true }))

          _ -> pure unit

      where chooseKey = flip elem [",", ";", " "]

-------------------------------------------------------------------------------

-- | TODO
noBubble
  :: forall i eff m
   . MonadAff (dom :: DOM, avar :: AVAR | eff) m
  => KeyboardEvent
  -> DSL i m eff Unit
noBubble = H.liftEff <<< preventDefault <<< keyboardEventToEvent


-- | TODO
options :: forall i m eff. (Options i eff -> Options i eff) -> State i m eff -> State i m eff
options f s = s { options = f s.options }


-- | TODO
rejectChosen :: forall i m eff. Int -> State i m eff -> State i m eff
rejectChosen k s = s { chosen = fromMaybe [] (deleteAt k s.chosen) }


-- | TODO
chooseOption :: forall i m eff. Int -> State i m eff -> State i m eff
chooseOption k s = case s.options.options !! k of
  Nothing -> s
  Just x  -> s { chosen  = s.chosen `snoc` x
               , buffer  = ""
               , options = o { visible  = false
                             , hoverIdx = Nothing }}
  where o = s.options


-- | TODO
chooseBuffer :: forall i m eff. State i m eff -> State i m eff
chooseBuffer s
  | s.buffer == "" = s
  | otherwise      =
    s { chosen  = s.chosen `snoc` s.config.parse s.buffer
      , buffer  = ""
      , options = o { visible  = false
                    , options  = []
                    , hoverIdx = Nothing }}
  where o = s.options


-- | TODO
updateSuggestions :: forall i m eff. Eq i => Array i -> State i m eff -> State i m eff
updateSuggestions xs s =
  s { options = case o.hoverIdx of
                  Nothing -> o { options = xs }
                  Just n_ -> let n = findIndex xs =<< o.options !! n_
                              in o { options = xs, hoverIdx = n }}
  where o = s.options


-- | TODO
clearBuffer :: forall i m eff. State i m eff -> State i m eff
clearBuffer s =
  s { buffer  = ""
    , options = { visible:     false
                , options:     []
                , hoverIdx:    Nothing
                , waitToHide:  Nothing
                , waitToShow:  Nothing }}


-- | TODO
resetState :: forall i m eff. State i m eff -> State i m eff
resetState s =
  s { buffer  = ""
    , chosen  = []
    , options = o { visible     = false
                  , options     = []
                  , hoverIdx    = Nothing
                  , waitToHide  = Nothing
                  , waitToShow  = Nothing }}
  where o = s.options


-- | TODO
bufferIsBlank :: forall r. { buffer :: String | r } -> Boolean
bufferIsBlank s = s.buffer == ""


-- | TODO
rejectLast :: forall i m eff. State i m eff -> State i m eff
rejectLast s = s { chosen = fromMaybe s.chosen (init s.chosen) }


-- | TODO
highlightPrev :: forall i m eff. State i m eff -> State i m eff
highlightPrev s
  | not s.options.visible = s
  | otherwise = s { options = case o.hoverIdx of
      Nothing -> o { hoverIdx = maybeBool (_ >= 0) (length o.options - 1) }
      Just n  -> o { hoverIdx = maybeBool (_ >= 0) (n - 1) }}
  where o = s.options


-- | TODO
highlightNext :: forall i m eff. State i m eff -> State i m eff
highlightNext s
  | not s.options.visible = s
  | otherwise = s { options = case o.hoverIdx of
      Nothing -> o { hoverIdx = maybeBool (_ < length o.options) 0 }
      Just n  -> o { hoverIdx = maybeBool (_ < length o.options) (n + 1) }}
  where o = s.options


findIndex :: forall a. Eq a => Array a -> a -> Maybe Int
findIndex xs x = fst <$> find ((x == _) <<< snd) (mapWithIndex Tuple xs)

module Pinboard.UI.TagInput
  ( Input
  , State
  , Query(..)
  , Output
  , component
  ) where

import Partial.Unsafe (unsafeCrashWith)

import Prelude
import Data.Array               (find, snoc, init, mapWithIndex, (!!), deleteAt, length)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype, over, wrap, unwrap)
import Data.Foldable            (elem)
import Data.Tuple               (Tuple(..), fst, snd)
import Data.Filterable          (maybeBool)
import Data.Traversable         (traverse)
import Data.Time.Duration       (Milliseconds(..))

import Control.Monad.Aff.AVar   (AVAR)
import Control.Monad.Aff.Class  (class MonadAff)

import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP

import DOM                      (DOM)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import DOM.Event.FocusEvent     as FE
import DOM.Event.KeyboardEvent  as KE

import Pinboard.UI.Debounce     (Debouncer)
import Pinboard.UI.Debounce     as D
import Pinboard.UI.Complete     as C

-------------------------------------------------------------------------------

type Input = Unit

newtype State m e =
  State
  { suggest       :: Array String -> String -> m (Array String) -- HMM
  , hideDelay     :: Milliseconds
  , suggestDelay  :: Milliseconds
  , selected      :: Array String
  , suggested     :: Array String
  , value         :: String
  , hidden        :: Boolean
  , highlighted   :: Maybe Int
  , waitToHide    :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | e))
  , waitToSuggest :: Maybe (Debouncer (avar :: AVAR, dom :: DOM | e)) }

derive instance newtypeState :: Newtype (State m e) _

data Query k
  = OnKey KE.KeyboardEvent k
  | OnBlur FE.FocusEvent k
  | OnInput String k
  | OnFocus FE.FocusEvent k
  | Reject Int k
  | Choose Int k

data Output
  = Changed (Array String)

type HTML    = H.ComponentHTML Query
type DSL m e = H.ComponentDSL (State m e) Query Output m

-------------------------------------------------------------------------------

component
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.component     -- ComponentSpec h s f i o m
  { initialState  -- i -> s
  , render        -- s -> h Void (f Unit)
  , eval          -- f ~> (ComponentDSL s f o m)
  , receiver }    -- i -> Maybe (f Unit)
  where
    initialState :: Input -> State m e
    initialState _ =
      State
      { suggest       : let q = C.commonSubsequences C.corpus
                         in \_ x -> pure (map fst (q x))
      , hideDelay     : Milliseconds 150.0
      , suggestDelay  : Milliseconds 150.0
      , selected      : []
      , suggested     : []
      , value         : ""
      , hidden        : true
      , highlighted   : Nothing
      , waitToHide    : Nothing
      , waitToSuggest : Nothing }

    render :: State _ _ -> HTML
    render (State s) =
      HH.div
        [ HP.class_ (H.ClassName "tags") ]
        [ renderSelections s.selected
        , HH.div_
            [ HH.input
                [ HP.placeholder ""
                , HP.type_ HP.InputText
                , HP.value s.value
                , HE.onBlur (HE.input OnBlur)
                , HE.onFocus (HE.input OnFocus)
                , HE.onKeyDown (HE.input OnKey)
                , HE.onValueInput (HE.input OnInput) ] ]
        , renderSuggestions s.suggested s.highlighted s.hidden ]
      where
        renderSelections [] = HH.text ""
        renderSelections xs =
          HH.ul [HP.class_ (H.ClassName "selected")] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Reject n)) ]
              [ HH.text x ]

        renderSuggestions _ _ true = HH.text ""
        renderSuggestions [] ix _  = HH.text ""
        renderSuggestions xs ix _  =
          HH.ul [HP.class_ (H.ClassName "suggested")] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Choose n))
              , HP.class_ (H.ClassName (if Just n == ix then "highlighted" else "")) ]
              [ HH.text x ]

    eval :: Query ~> DSL m e
    eval q = case q of
      Reject n k -> k <$
        H.modify (removeTag n)

      Choose n k -> k <$ do
        H.modify (chooseSuggestion n)
        s <- H.gets unwrap
        H.raise (Changed s.selected)

      OnInput input k -> k <$ do
        -- wait a tick for user to stop typing before computing suggestions
        s <- H.gets unwrap
        x <- case s.waitToSuggest of
                  Nothing -> D.create s.suggestDelay
                  Just _x -> D.reset _x s.suggestDelay

        H.modify (over State (_ { value = input, hidden = false }))
        H.modify (over State (_ { waitToSuggest = Just x }))

        H.fork $ D.whenQuiet x do
          matches <- H.lift (s.suggest s.selected input)
          H.modify (updateSuggested matches)

      OnBlur e k -> k <$ do
        -- wait a tick for user to stop clicking before hiding suggestions
        s <- H.gets unwrap
        x <- case s.waitToHide of
                  Nothing -> D.create s.hideDelay
                  Just _x -> D.reset _x s.hideDelay
        H.modify (over State (_ { waitToHide = Just x }))

        H.fork $ D.whenQuiet x do
          H.modify (pushBuffer <<< over State (_ { waitToHide = Nothing, suggested = [] }))

      OnFocus e k -> k <$ do
        -- if we were about to hide suggestions, don't
        _ <- traverse D.cancel =<< H.gets (_.waitToHide <<< unwrap)
        H.modify (over State (_ { waitToHide = Nothing }))

      OnKey e k -> k <$ do
        blank <- H.gets ((\s -> s.value == "") <<< unwrap)
        s     <- H.gets unwrap
        case KE.key e of
             -- ⌘ ⌫   clears everything
             --
             -- ⌫     when buffer is empty, removes most recent entry
             --       otherwise, removes char to the left of cursor
             --
             -- ↩︎     when suggestion highlighted, choose it
             --       otherwise, add buffer text
             --
             -- ,     add buffer text (do nothing if buffer is empty)
             -- ;     add buffer text (do nothing if buffer is empty)
             -- Space add buffer text (do nothing if buffer is empty)
             --
             -- Esc   removes highlight, hides suggestions
             --
             -- Tab   when buffer is empty, focus next field
             --       when suggestions are empty, add buffer text
             --       when only one suggestion, choose it
             --       otherwise, same as right arrow
             --
             -- Shift-Tab
             --       when a suggestion is highlighted, highlight prev
             --       otherwise, focus previous field
             --
             -- ←     when a suggestion is highlighted, highlight prev
             -- →     when a suggestion is highlighted, highlight next
             --
             -- text  update suggestions and show them
             "Backspace"
               | KE.metaKey e     -> H.modify resetState
               | otherwise        -> when blank (H.modify removeLast)
             "Enter"              -> noBubble e *> case s.highlighted of
                                       Just n  -> H.modify (chooseSuggestion n)
                                       Nothing -> H.modify pushBuffer
             x | pb x             -> H.modify pushBuffer  *> noBubble e
             "Escape"             -> H.modify clearBuffer *> noBubble e
             "Tab"
               | KE.shiftKey e    -> H.modify highlightPrev *> noBubble e
               | otherwise        -> H.modify highlightNext *> noBubble e
             "ArrowLeft"
               | not KE.shiftKey e &&
                 not KE.metaKey e &&
                 not KE.altKey e  -> H.modify highlightPrev *> noBubble e
             "ArrowRight"
               | not KE.shiftKey e &&
                 not KE.metaKey e &&
                 not KE.altKey e  -> H.modify highlightNext *> noBubble e
             "ArrowDown"
               | s.hidden         -> H.modify showSuggestions
             _                    -> pure unit
        where pb x = x `elem` [",", ";", " "]

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

-------------------------------------------------------------------------------

noBubble
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => KE.KeyboardEvent
  -> DSL m e Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.keyboardEventToEvent

showSuggestions :: forall e x. State e x -> State e x
showSuggestions (State s) = State (s { hidden = false })

hideSuggestions :: forall e x. State e x -> State e x
hideSuggestions (State s) = State (s { hidden = true })

highlightPrev :: forall e x. State e x -> State e x
highlightPrev (State s)
  | s.hidden  = State s
  | otherwise = State $
    case s.highlighted of
         Nothing -> s { highlighted = maybeBool (_ >= 0) (length s.suggested - 1) }
         Just n  -> s { highlighted = maybeBool (_ >= 0) (n - 1) }

highlightNext :: forall e x. State e x -> State e x
highlightNext (State s)
  | s.hidden  = State s
  | otherwise = State $
    case s.highlighted of
         Nothing -> s { highlighted = maybeBool (_ < length s.suggested) 0 }
         Just n  -> s { highlighted = maybeBool (_ < length s.suggested) (n + 1) }

updateSuggested :: forall e x. Array String -> State e x -> State e x
updateSuggested ss (State s) = State $ case s.highlighted of
  Nothing -> s { waitToSuggest = Nothing, suggested = ss }
  Just n  ->
    -- if currently highlighted item is also in the new
    -- set of suggestions, then keep it highlighted
    let m = findIndex ss =<< s.suggested !! n
       in s { waitToSuggest = Nothing, suggested = ss, highlighted = m }

findIndex :: forall a. Eq a => Array a -> a -> Maybe Int
findIndex xs x = fst <$> find ((x == _) <<< snd) (mapWithIndex Tuple xs)

resetState :: forall e x. State e x -> State e x
resetState (State s) = State s { suggested = [], selected = [], value = "" }

clearBuffer :: forall e x. State e x -> State e x
clearBuffer (State s) = State s { suggested = [], value = "" }

pushBuffer :: forall e x. State e x -> State e x
pushBuffer (State s)
  | s.value == "" = State s { hidden = true }
  | isDuplicate s s.value = State s { hidden = true, suggested = [], value = "" }
  | otherwise = State s { selected = s.selected `snoc` s.value, suggested = [], value = "" }

removeTag :: forall e x. Int -> State e x -> State e x
removeTag n (State s) = State s { selected = fromMaybe s.selected (deleteAt n s.selected) }

isDuplicate :: forall r. { selected :: Array String | r } -> String -> Boolean
isDuplicate s x = x `elem` s.selected

chooseSuggestion :: forall e x. Int -> State e x -> State e x
chooseSuggestion n (State s) = case s.suggested !! n of
  Nothing -> State s
  Just x
    | isDuplicate s x -> State s { suggested = [], highlighted = Nothing, value = "" }
    | otherwise       -> State s { selected     = s.selected `snoc` x
                                 , suggested    = []
                                 , highlighted  = Nothing
                                 , value        = "" }

removeLast :: forall e x. State e x -> State e x
removeLast (State s) = State s { selected = fromMaybe [] (init s.selected) }

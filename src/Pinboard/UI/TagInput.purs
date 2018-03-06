module Pinboard.UI.TagInput
  ( Input
  , State
  , Query(..)
  , Output
  , component
  ) where

import Partial.Unsafe (unsafeCrashWith)

import Prelude
import Data.Array               (snoc, init, mapWithIndex, (!!), deleteAt)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype, over, wrap, unwrap)
import Data.Foldable            (elem)
import Data.Traversable         (traverse)
import Data.Time.Duration       (Milliseconds(..))

import Control.Monad.Aff        (Aff)
import Control.Monad.Aff.AVar   (AVAR, takeVar, makeEmptyVar)
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

import Pinboard.UI.Debounce     (Debouncer, newDebouncer, resetDebouncer, cancelDebouncer, whenQuiet)

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

type Output = Void

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
      { suggest       : \_ _ -> pure ["one", "two", "ten"]
      , hideDelay     : Milliseconds 150.0
      , suggestDelay  : Milliseconds 150.0
      , selected      : []
      , suggested     : []
      , value         : ""
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
        , renderSuggestions s.suggested ]
      where
        renderSelections [] = HH.text ""
        renderSelections xs =
          HH.ul [HP.class_ (H.ClassName "selected")] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Reject n)) ]
              [ HH.text x ]

        renderSuggestions [] = HH.text ""
        renderSuggestions xs =
          HH.ul [HP.class_ (H.ClassName "suggested")] $ flip mapWithIndex xs \n x ->
            HH.li
              [ HE.onClick (HE.input_ (Choose n)) ]
              [ HH.text x ]

    eval :: Query ~> DSL m e
    eval q = case q of
      Reject n k -> k <$
        H.modify (removeItem n)

      Choose n k -> k <$
        H.modify (selectItem n)

      OnInput input k -> k <$ do
        -- wait a tick for user to stop typing before computing suggestions
        s <- H.gets unwrap
        x <- case s.waitToSuggest of
                  Nothing -> newDebouncer s.suggestDelay
                  Just _x -> resetDebouncer _x s.suggestDelay

        H.modify (over State (_ { value = input }))
        H.modify (over State (_ { waitToSuggest = Just x }))

        -- this forks to run in the background
        H.fork $ whenQuiet x do
          matches <- H.lift (s.suggest s.selected input)
          H.modify (over State (_ { waitToSuggest = Nothing, suggested = matches }))

      OnBlur e k -> k <$ do
        -- wait a tick for user to stop clicking before hiding suggestions
        s <- H.gets unwrap
        x <- case s.waitToHide of
                  Nothing -> newDebouncer s.hideDelay
                  Just _x -> resetDebouncer _x s.hideDelay
        H.modify (over State (_ { waitToHide = Just x }))

        -- this forks to run in the background
        H.fork $ whenQuiet x do
          H.modify (insertItem <<< over State (_ { waitToHide = Nothing, suggested = [] }))

      OnFocus e k -> k <$ do
        -- if we were about to hide suggestions, don't
        w <- H.gets (_.waitToHide <<< unwrap)
        _ <- traverse cancelDebouncer w
        H.modify (over State (_ { waitToHide = Nothing }))

      OnKey e k -> k <$ do
        blank <- H.gets ((\s -> s.value == "") <<< unwrap)
        case KE.key e of
             "Backspace"
               | KE.metaKey e -> H.modify reset
               | otherwise    -> when blank (H.modify removeLast)
             "Escape"         -> H.modify clear
             x | split x      -> H.modify insertItem *> noBubble e
             _                -> pure unit

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

-------------------------------------------------------------------------------

noBubble
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => KE.KeyboardEvent
  -> DSL m e Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.keyboardEventToEvent

split :: String -> Boolean
split x = x `elem` ["Enter", ",", ";", " "]

reset :: forall e x. State e x -> State e x
reset (State s) = State s { selected = [], value = "" }

clear :: forall e x. State e x -> State e x
clear (State s) = State s { suggested = [], value = "" }

insertItem :: forall e x. State e x -> State e x
insertItem (State s)
  | s.value == "" = State s
  | s.value `elem` s.selected = State s { suggested = [], value = "" }
  | otherwise = State s { selected = s.selected `snoc` s.value, suggested = [], value = "" }

removeItem :: forall e x. Int -> State e x -> State e x
removeItem n (State s) = State s { selected = fromMaybe s.selected (deleteAt n s.selected) }

selectItem :: forall e x. Int -> State e x -> State e x
selectItem n (State s) = case s.suggested !! n of
  Nothing -> State s
  Just x  -> State s { selected = s.selected `snoc` x, suggested = [], value = "" }

removeLast :: forall e x. State e x -> State e x
removeLast (State s) = State s { selected = fromMaybe [] (init s.selected) }

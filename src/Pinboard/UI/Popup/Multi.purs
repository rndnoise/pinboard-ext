module Pinboard.UI.Popup.Multi where

import Prelude
import Data.Array               (mapWithIndex)
import Data.Either              (Either)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype)
import Control.Monad.Aff.AVar   (AVAR)
import Control.Monad.Aff.Class  (class MonadAff)
import Network.HTTP.Affjax      (AJAX)
import DOM                      (DOM)
import DOM.Event.Types          as ET
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP

import Chrome.Tabs.Tab          as CT
import Pinboard.API             (Error)
import Pinboard.UI.HTML         (class_)
import Pinboard.UI.TagInput     as TI

-------------------------------------------------------------------------------

newtype State = State
  { tabs    :: Array CT.Tab
  , tags    :: Array String
  , toRead  :: Boolean
  , private :: Boolean
  , replace :: Boolean
  , chosen  :: Array Boolean
  , status  :: Unit }

derive instance newtypeState :: Newtype State _

data Query i k
  = Save ET.MouseEvent k
  | OnTitle Int String k
  | OnChoose Int Boolean k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | ApiPostAdd (Either Error Unit) k
  | FromTagWidget (TI.Output i) k

type Input = Array CT.Tab
type Output = Void

data Slot = TagSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML i m = H.ParentHTML (Query i) (TI.Query i) Slot m
type DSL i m  = H.ParentDSL State (Query i) (TI.Query i) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall i e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM | e) m
  => Eq i
  => TI.Config i m
  -> H.Component HH.HTML (Query i) Input Output m
component cfg =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input -> State
    initialState ts = State
      { tabs:     ts
      , tags:     []
      , toRead:   false
      , private:  true
      , replace:  true
      , chosen:   []
      , status:   unit }

    render :: State -> HTML i m
    render (State s) =
      HH.form [ class_ "multi" ]
        [ HH.div [ class_ "top" ]
            [ HH.label [ HP.for "tags", class_ "select" ]
              [ HH.text "Tags:"
              , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagWidget) ]

            , HH.label [ HP.for "toread", class_ "checkbox" ]
              [ HH.input
                [ HP.id_ "toread"
                , HP.type_ HP.InputCheckbox
                , HP.checked true
                , HE.onChecked (HE.input OnToRead) ]
              , HH.text "Read later" ]
            , HH.label [ HP.for "private", class_ "checkbox" ]
              [ HH.input
                [ HP.id_ "private"
                , HP.type_ HP.InputCheckbox
                , HP.checked true
                , HE.onChecked (HE.input OnPrivate) ]
              , HH.text "Private" ]
            , HH.label [ HP.for "replace", class_ "checkbox" ]
              [ HH.input
                [ HP.id_ "replace"
                , HP.type_ HP.InputCheckbox
                , HP.checked true
                , HE.onChecked (HE.input OnReplace) ]
              , HH.text "Replace" ]

            , HH.button [ class_ "primary", HE.onClick (HE.input Save) ]
              [ HH.text "Save" ] ]

        , HH.ul_ $ flip mapWithIndex s.tabs \n tab ->
            HH.li_
              [ HH.label [ HP.for ("t" <> show n) ]
                [ HH.input
                  [ HP.id_ ("t" <> show n)
                  , HP.type_ HP.InputCheckbox
                  , HP.checked true
                  , HE.onChecked (HE.input (OnChoose n)) ]
                , HH.img [ HP.src (fromMaybe "" (CT.favIconUrl tab)) ]
                , HH.input
                  [ class_ "title"
                  , HP.type_ HP.InputText
                  , HP.value (fromMaybe "" (CT.title tab))
                  , HE.onValueInput (HE.input (OnTitle n)) ]
                , HH.div [ class_ "url"   ] [ HH.text (fromMaybe "" (CT.url tab)) ] ] ]
        ]

    eval :: Query i ~> DSL i m
    eval q = case q of
      Save e k -> pure k
      OnTitle n s k -> pure k
      OnChoose n b k -> pure k
      OnToRead b k -> pure k
      OnPrivate b k -> pure k
      OnReplace b k -> pure k
      ApiPostAdd res k -> pure k
      FromTagWidget o k -> k <$
        case o of
          TI.OnChosen x -> pure unit

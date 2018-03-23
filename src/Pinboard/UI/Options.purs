module Pinboard.UI.Options where

import Prelude
import DOM                        (DOM)
import DOM.Event.Types            as ET
import Control.Monad.Aff.Class    (class MonadAff)
import Control.Monad.Jax.Class    (class MonadJax)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Eff          (Eff)
import Data.Either                (Either)
import Data.Maybe                 (Maybe(..))
import Data.Newtype               (class Newtype, over)
import Data.StrMap                (fromFoldable)
import Data.Tuple                 (Tuple(..))
import Halogen                    as H
import Halogen.Aff                as HA
import Halogen.HTML               as HH
import Halogen.HTML.Events        as HE
import Halogen.HTML.Properties    as HP
import Halogen.VDom.Driver        as HV
import Network.HTTP.Affjax        (AJAX)

import Pinboard.UI.Internal.HTML  (class_)
import Pinboard.API               (AuthToken, Error, Tag)
import Chrome.FFI                 (CHROME)
import Chrome.Storage.Local       as LS

-- | This is executed when the config page is shown
main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  --   <- HV.runUI (H.hoist (flip runReaderT "") component) unit body
  pure unit

data Query k
  = Init k
  | Exit k
  | Save ET.MouseEvent k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | OnAuthToken String k
  | ApiTagsGet (Either Error (Array (Tuple Tag Number))) k

newtype State = State
  { toRead    :: Boolean
  , private   :: Boolean
  , replace   :: Boolean
  , authToken :: String }

derive instance stateNewtype :: Newtype State _

type Input  = Unit
type Output = Unit

type HTML  = H.ComponentHTML Query
type DSL m = H.ComponentDSL State Query Output m

component
  :: forall eff m
   . MonadAff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME | eff)) m
  => MonadJax m
  => MonadAsk AuthToken m
  => H.Component HH.HTML Query Input Output m
component =
  H.lifecycleComponent
  { initialState
  , render
  , eval
  , receiver:    const Nothing
  , finalizer:   Just (H.action Exit)
  , initializer: Just (H.action Init) }
  where
    initialState :: Input -> State
    initialState _ =
      State
      { toRead:     false
      , private:    false
      , replace:    false
      , authToken:  "" }

    render :: State -> HTML
    render (State s) = HH.form_
      [ HH.label [class_ "text"]
        [ HH.text "API Token"
        , HH.input
          [ HP.type_ HP.InputText
          , HP.value s.authToken
          , HE.onValueInput (HE.input OnAuthToken) ] ]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked s.toRead
          , HE.onChecked (HE.input OnToRead) ]
        , HH.text "Check 'to read' by default" ]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked s.private
          , HE.onChecked (HE.input OnPrivate) ]
        , HH.text "Check 'private' by default" ]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked s.replace
          , HE.onChecked (HE.input OnReplace) ]
        , HH.text "Check 'replace' by default" ]

      , HH.button [class_ "primary", HE.onClick (HE.input Save)]
        [ HH.text "Save" ]
      ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k -> pure k

      Exit k -> pure k

      Save e k -> k <$ do
        State s <- H.get
        pure unit {-
        lift $ LS.set (fromFoldable
                       [ Tuple "toRead"    (LS.toStorable s.toRead)
                       , Tuple "private"   (LS.toStorable s.private)
                       , Tuple "replace"   (LS.toStorable s.replace)
                       , Tuple "authToken" (LS.toStorable s.authToken) ])-}

      OnToRead value k -> k <$ do
        H.modify (over State (_ { toRead = value }))

      OnPrivate value k -> k <$ do
        H.modify (over State (_ { private = value }))

      OnReplace value k -> k <$ do
        H.modify (over State (_ { replace = value }))

      OnAuthToken value k -> k <$ do
        H.modify (over State (_ { authToken = value }))

      ApiTagsGet res k -> pure k


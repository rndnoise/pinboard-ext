module Pinboard.UI.Options where

import Prelude
import Control.Monad.Aff.Class    (class MonadAff)
import Control.Monad.Jax.Class    (class MonadJax)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Eff          (Eff)
import Data.Either                (Either)
import Data.Identity              (Identity)
import Data.Maybe                 (Maybe(..))
import Data.Newtype               (class Newtype, unwrap)
import Data.Tuple                 (Tuple)
import Halogen                    as H
import Halogen.Aff                as HA
import Halogen.HTML               as HH
import Halogen.HTML.Events        as HE
import Halogen.HTML.Properties    as HP
import Halogen.VDom.Driver        as HV
import Network.HTTP.Affjax        (AJAX)

import Chrome.FFI                 (CHROME)
import Pinboard.API               (Error)
import Pinboard.Config            (Config, Defaults, Tag, loadConfig, saveConfig)
import Pinboard.UI.Internal.HTML  (class_)

-- | This is executed when the config page is shown
main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  conf <- loadConfig
  _    <- HV.runUI component conf body
  pure unit

data Query k
  = OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | OnAuthToken String k
  | ApiTagsGet (Either Error (Array (Tuple Tag Number))) k

newtype State = State
  { config :: Config Tag Identity
  , status :: Status }

derive instance newtypeState :: Newtype State _

data Status
  = Info String
  | Error String
  | Success String

type Input  = Config Tag Identity
type Output = Unit

type HTML  = H.ComponentHTML Query
type DSL m = H.ComponentDSL State Query Output m

component
  :: forall eff m
   . MonadAff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME | eff)) m
  => MonadJax m
  => H.Component HH.HTML Query Input Output m
component =
  H.component
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input -> State
    initialState config = State { config, status: Info "message" }

    render :: State -> HTML
    render (State { config, status }) = HH.form_
      [ HH.label [class_ "text"]
        [ HH.text "API token -- see "
        , HH.a [HP.href "https://pinboard.in/settings/password"] [HH.text "pinboard.in/settings"]
        , HH.text " to view yours:"
        , HH.input
          [ HP.type_ HP.InputText
          , HP.value config.authToken
          , HE.onValueInput (HE.input OnAuthToken) ]
        , case status of
            Info msg    -> HH.span [class_ ""] [ HH.text msg ]
            Error msg   -> HH.span [class_ ""] [ HH.text msg ]
            Success msg -> HH.span [class_ ""] [ HH.text msg ]]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked config.defaults.readLater
          , HE.onChecked (HE.input OnToRead) ]
        , HH.text "Mark " , HH.b_ [ HH.text "read later" ] , HH.text " by default" ]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked config.defaults.private
          , HE.onChecked (HE.input OnPrivate) ]
        , HH.text "Make bookmarks " , HH.b_ [ HH.text "private" ] , HH.text " by default" ]

      , HH.label [class_ "checkbox"]
        [ HH.input
          [ HP.type_ HP.InputCheckbox
          , HP.checked config.defaults.replace
          , HE.onChecked (HE.input OnReplace) ]
        , HH.b_ [ HH.text "Replace" ] , HH.text " existing bookmarks by default" ]]

    eval :: Query ~> DSL m
    eval q = case q of
      OnToRead value k -> k <$ do
        H.modify (defaults (_ { readLater = value }))
        H.liftAff <<< saveConfig =<< H.gets (_.config <<< unwrap)

      OnPrivate value k -> k <$ do
        H.modify (defaults (_ { private = value }))
        H.liftAff <<< saveConfig =<< H.gets (_.config <<< unwrap)

      OnReplace value k -> k <$ do
        H.modify (defaults (_ { replace = value }))
        H.liftAff <<< saveConfig =<< H.gets (_.config <<< unwrap)

      OnAuthToken value k -> k <$ do
        H.modify (config (_ { authToken = value }))
        H.liftAff <<< saveConfig =<< H.gets (_.config <<< unwrap)

      ApiTagsGet res k -> pure k

config :: (Config Tag Identity -> Config Tag Identity) -> State -> State
config f (State s) = State s { config = f (s.config) }

defaults :: (Defaults -> Defaults) -> State -> State
defaults f = config (\c -> c { defaults = f (c.defaults) })

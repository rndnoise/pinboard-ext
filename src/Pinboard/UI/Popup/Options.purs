module Pinboard.UI.Popup.Options
  ( component
  , Input
  , Output
  , Query
  , State
  , Status
  ) where

import Prelude
import Chrome.FFI                     (CHROME)
import Control.Monad.Aff.AVar         (AVAR)
import Control.Monad.Aff.Class        (class MonadAff)
import Control.Monad.Jax.Class        (class MonadJax)
import Data.Either                    (Either(..))
import Data.Maybe                     (Maybe(..))
import Data.Time.Duration             (Milliseconds(..))
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Network.HTTP.Affjax            (AJAX)
import Pinboard.API                   (authToken, Error(..), tagsGet)
import Pinboard.UI.Component.Debounce (Debouncer)
import Pinboard.UI.Component.Debounce as D
import Pinboard.Config                (Config, Defaults, saveConfig, saveTags)
import Pinboard.UI.Internal.HTML      as PH

-------------------------------------------------------------------------------

data Query k
  = OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | OnAuthToken String k

type State m eff =
  { config   :: Config m
  , status   :: Status
  , waitAuth :: Maybe (Debouncer (ajax :: AJAX, chrome :: CHROME | eff)) }

data Status
  = Info String
  | Error String
  | Success String

type Input m  = Config m
type Output m = Config m

type HTML      = H.ComponentHTML Query
type DSL m eff = H.ComponentDSL (State m eff) Query (Output m) m

-------------------------------------------------------------------------------

component
  :: forall eff m
   . MonadAff (ajax :: AJAX, avar :: AVAR, chrome :: CHROME | eff) m
  => MonadJax m
  => H.Component HH.HTML Query (Input m) (Output m) m
component =
  H.component
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input m -> State m eff
    initialState c =
      { config:   c
      , waitAuth: Nothing
      , status:   Info (if show c.authToken /= ""
                        then "Options"
                        else "API token is required") }

    render :: State m eff -> HTML
    render s =
      HH.form
      [ HP.id_ "options" ]
      [ case s.status of
          Info msg    -> HH.div [ PH.class_ "status light"  ] [ HH.text msg ]
          Error msg   -> HH.div [ PH.class_ "status danger" ] [ HH.text msg ]
          Success msg -> HH.div [ PH.class_ "status success"] [ HH.text msg ]

      , HH.div
        [ PH.class_ "urgh" ]
        [ HH.label
          [ PH.class_ "text" ]
          [ HH.text "API token (see "
          , HH.a [ HP.href "https://pinboard.in/settings/password" ] [ HH.text "pinboard.in/settings" ]
          , HH.text " to view yours):"
          , HH.input
            [ HP.type_ HP.InputText
            , HP.value (show s.config.authToken)
            , HP.spellcheck false
            , HP.autocomplete false
            , HP.required true
            , HE.onValueInput (HE.input OnAuthToken)
            ]
          ]

        , HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.config.defaults.readLater
            , HE.onChecked (HE.input OnToRead)
            ]
          , HH.text "Mark "
          , HH.b_ [ HH.text "read later" ]
          , HH.text " by default"
          ]

        , HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.config.defaults.private
            , HE.onChecked (HE.input OnPrivate)
            ]
          , HH.text "Make bookmarks "
          , HH.b_ [ HH.text "private" ]
          , HH.text " by default"
          ]

        , HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.config.defaults.replace
            , HE.onChecked (HE.input OnReplace)
            ]
          , HH.b_ [ HH.text "Replace" ]
          , HH.text " existing bookmarks by default"
          ]
        ]
      ]

    eval :: Query ~> DSL m eff
    eval q = case q of
      OnToRead value k -> k <$ do
        H.modify (defaults (_{ readLater = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnPrivate value k -> k <$ do
        H.modify (defaults (_{ private = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnReplace value k -> k <$ do
        H.modify (defaults (_{ replace = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnAuthToken value k -> k <$ do
        H.modify (config (_{ authToken = authToken value }))

        s <- H.get
        w <- case s.waitAuth of
               Nothing -> D.create   authDelay
               Just _w -> D.reset _w authDelay
        H.modify (_{ waitAuth = Just w })

        c <- H.gets _.config
        H.liftAff (saveConfig c)
        H.raise c

        H.fork $ D.whenQuiet w do
          H.modify (_{ status = Info "Checking...", waitAuth = Nothing })

          H.lift (tagsGet (authToken value)) >>= unwrapResponse \tags -> do
            H.modify (_{ status = Success "Successfully authenticated" })
            H.modify (config (\x -> x { tags = c.reloadTags tags }))

            H.liftAff (saveTags tags)
            H.raise =<< H.gets _.config

    authDelay :: Milliseconds
    authDelay = Milliseconds 500.0


config :: forall m eff. (Config m -> Config m) -> State m eff -> State m eff
config f s = s { config = f (s.config) }


defaults :: forall m eff. (Defaults -> Defaults) -> State m eff -> State m eff
defaults f s = config (\c -> c { defaults = f (c.defaults) }) s


unwrapResponse
  :: forall m eff a
   . (a -> DSL m eff Unit)
  -> Either Error a
  -> DSL m eff Unit
unwrapResponse f (Right a) = f a
unwrapResponse _ (Left e) =
  case e of
    JsonError msg  -> H.modify (_{ status = Error ("JSON: "  <> msg) })
    UserError msg  -> H.modify (_{ status = Error ("Server: " <> msg) })
    HttpError 401  -> H.modify (_{ status = Error "Incorrect API token" })
    HttpError code -> H.modify (_{ status = Error ("HTTP " <> show code) })

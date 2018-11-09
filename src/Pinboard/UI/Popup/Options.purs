module Pinboard.UI.Popup.Options
  ( component
  , Input
  , Output
  , Query(OnBlur, OnFocus)
  , State
  , Status
  ) where

import Prelude
import Effect.Aff.Class               (class MonadAff)
import Effect.Aff.Jax.Class           (class MonadJax)
import Data.Either                    (Either(..))
import Data.Maybe                     (Maybe(..))
import Data.Time.Duration             (Milliseconds(..))
import Data.Foldable                  (traverse_)
import Web.HTML.HTMLElement           (focus) as DOM
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Pinboard.API                   (authToken, Error(..), tagsGet)
import Pinboard.UI.Component.Debounce (Debouncer)
import Pinboard.UI.Component.Debounce as D
import Pinboard.UI.Internal.HTML      as PH
import Pinboard.Config                (Config, Defaults, saveConfig, saveTags)

-------------------------------------------------------------------------------

data Query k
  = OnBlur k
  | OnFocus k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | OnAuthToken String k

type State m =
  { config   :: Config m
  , status   :: Status
  , waitAuth :: Maybe Debouncer }

data Status
  = Info String
  | Error String
  | Success String

type Input m  = Config m
type Output m = Config m

type HTML  = H.ComponentHTML Query
type DSL m = H.ComponentDSL (State m) Query (Output m) m

-------------------------------------------------------------------------------

component
  :: forall m
   . MonadAff m
  => MonadJax m
  => H.Component HH.HTML Query (Input m) (Output m) m
component =
  H.component
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input m -> State m
    initialState c =
      { config:   c
      , waitAuth: Nothing
      , status:   Info (if show c.authToken /= ""
                        then "Options"
                        else "API token is required") }

    render :: State m -> HTML
    render s =
      HH.form
      [ HP.id_ "options" ]
      [ case s.status of
          Info msg    -> HH.div [ PH.class_ "status light"  ] [ HH.text msg ]
          Error msg   -> HH.div [ PH.class_ "status danger" ] [ HH.text msg ]
          Success msg -> HH.div [ PH.class_ "status success"] [ HH.text msg ]

      , HH.div
        [ PH.class_ "urgh" ]
        [ HH.div
          [ PH.class_ "message" ]
          [ HH.text "See "
          , HH.a
            [ HP.target "_blank"
            , HP.href "https://pinboard.in/settings/password" ]
            [ HH.text "pinboard.in/settings" ]
          , HH.text " to retrieve your API token"
          ]
        , HH.label
          [ PH.class_ "text" ]
          [ HH.input
            [ PH.class_ "icon-key"
            , HP.ref (H.RefLabel "token")
            , HP.type_ HP.InputText
            , HP.placeholder "username:hexadecimal string"
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

    eval :: Query ~> DSL m
    eval q = case q of
      OnBlur k -> pure k

      OnFocus k -> k <$ do
        input <- H.getHTMLElementRef (H.RefLabel "token")
        H.liftEffect $ traverse_ DOM.focus input

      OnToRead value k -> k <$ do
        H.modify_ (defaults (_{ readLater = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnPrivate value k -> k <$ do
        H.modify_ (defaults (_{ private = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnReplace value k -> k <$ do
        H.modify_ (defaults (_{ replace = value }))
        c <- H.gets _.config
        _ <- H.liftAff (saveConfig c)
        H.raise c

      OnAuthToken value k -> k <$ do
        H.modify_ (config (_{ authToken = authToken value }))

        s <- H.get
        w <- case s.waitAuth of
               Nothing -> D.create   authDelay
               Just _w -> D.reset _w authDelay
        H.modify_ (_{ waitAuth = Just w })

        c <- H.gets _.config
        H.liftAff (saveConfig c)
        H.raise c

        H.fork $ D.whenQuiet w do
          H.modify_ (_{ status = Info "Checking...", waitAuth = Nothing })

          H.lift (tagsGet (authToken value)) >>= unwrapResponse \tags -> do
            H.modify_ (_{ status = Success "Successfully authenticated" })
            H.modify_ (config (\x -> x { tags = c.reloadTags tags }))

            H.liftAff (saveTags tags)
            H.raise =<< H.gets _.config

    authDelay :: Milliseconds
    authDelay = Milliseconds 500.0


config :: forall m. (Config m -> Config m) -> State m -> State m
config f s = s { config = f (s.config) }


defaults :: forall m. (Defaults -> Defaults) -> State m -> State m
defaults f s = config (\c -> c { defaults = f (c.defaults) }) s


unwrapResponse :: forall m a. (a -> DSL m Unit) -> Either Error a -> DSL m Unit
unwrapResponse f (Right a) = f a
unwrapResponse _ (Left e) =
  case e of
    JsonError msg  -> H.modify_ (_{ status = Error ("JSON: "  <> msg) })
    UserError msg  -> H.modify_ (_{ status = Error ("Server: " <> msg) })
    HttpError 401  -> H.modify_ (_{ status = Error "Incorrect API token" })
    HttpError code -> H.modify_ (_{ status = Error ("HTTP " <> show code) })

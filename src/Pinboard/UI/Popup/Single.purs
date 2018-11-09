module Pinboard.UI.Popup.Single
  ( component
  , Input
  , Output
  , Query(OnBlur, OnFocus)
  , State
  , Status
  ) where

import Prelude
import WebExtensions.Tabs.Tab         (Tab, title, url) as CT
-- ort Control.Comonad                (extract)
import Effect.Aff.Class               (class MonadAff)
import Effect.Aff.Jax.Class           (class MonadJax)
import Web.Event.Event                (Event, preventDefault)
import Web.UIEvent.MouseEvent         (toEvent)
import Data.Array                     (uncons)
import Data.DateTime                  (DateTime)
import Data.Either                    (Either(..), either)
import Data.Formatter.DateTime        (formatDateTime)
import Data.Maybe                     (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid                    (guard)
import Data.Tuple                     (Tuple(..))
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Pinboard.Config                (Config, Tag)
import Pinboard.UI.Component.TagInput as TI
import Pinboard.UI.Internal.HTML      as PH
import Pinboard.UI.Internal.Popup     (closePopup)

import Pinboard.API
  ( Error(..)
  , postsGet
  , postsAdd
  , postsDelete
  , addOptions
  , getOptions )

-------------------------------------------------------------------------------

type State m =
  { title     :: String
  , url       :: String
  , desc      :: String
  , tags      :: Array String
  , readLater :: Boolean
  , private   :: Boolean
  , time      :: Maybe DateTime
  , config    :: Config m
  , status    :: Status }

data Status
  = Info String
  | Error String
  | Success String

data Query m k
  = Init k
  | Recv (Input m) k
  | OnBlur k
  | OnFocus k
  | OnUrl String k
  | OnTitle String k
  | OnDesc String k
  | OnReadLater Boolean k
  | OnPrivate Boolean k
  | Save Event k
  | Delete Event k
  | FromTagInput (TI.Output Tag) k

type Input m = Tuple (Config m) (Maybe CT.Tab)
type Output  = Void

data Slot = TagSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML (Query m) (TI.Query Tag m) Slot m
type DSL m  = H.ParentDSL (State m) (Query m) (TI.Query Tag m) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall m
   . MonadAff m
  => MonadJax m
  => H.Component HH.HTML (Query m) (Input m) Output m
component =
  H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , finalizer:   Nothing
  , initializer: Just (H.action Init)
  , receiver:    \i -> Just (Recv i unit) }
  where
    initialState :: Input m -> State m
    initialState (Tuple config tab) =
      { title:      fromMaybe "" (CT.title =<< tab)
      , url:        fromMaybe "" (CT.url   =<< tab)
      , desc:       ""
      , tags:       []
      , readLater:  config.defaults.readLater
      , private:    config.defaults.private
      , time:       Nothing
      , status:     Info "Checking..."
      , config }

    render :: State m -> HTML m
    render s =
      HH.form
      [ HP.id_ "single" ]
      [ case s.status of
          Info x    -> HH.div [PH.class_ "status light"   ] [ HH.text x ]
          Error x   -> HH.div [PH.class_ "status danger"  ] [ HH.text x ]
          Success x -> HH.div [PH.class_ "status success" ] [ HH.text x ]

      , HH.div
        [ PH.class_ "urgh" ] $
        [ HH.label
          [ PH.class_ "text" ]
          [ HH.slot TagSlot TI.component
              (Tuple s.config.tags (map s.config.tags.parse s.tags))
              (HE.input FromTagInput)
          ]

        , HH.label
          [ PH.class_ "textarea" ]
          [ HH.text "Description:"
          , HH.textarea
            [ HP.value s.desc
            , HP.required false
            , HP.spellcheck true
            , HE.onValueInput (HE.input OnDesc)
            ]
          ]

        , HH.label
          [ PH.class_ "text" ]
          [ HH.input
            [ PH.class_ "icon-title"
            , HP.type_ HP.InputText
            , HP.value s.title
            , HP.required true
            , HP.spellcheck false
            , HP.autocomplete false
            , HE.onValueInput (HE.input OnTitle)
            ]
          ]

        , HH.label
          [ PH.class_ "text" ]
          [ HH.input
            [ PH.class_ "icon-url"
            , HP.type_ HP.InputUrl
            , HP.value s.url
            , HP.required true
            , HP.spellcheck false
            , HP.autocomplete false
            , HE.onValueInput (HE.input OnUrl)
            ]
          ]
        ]
        <>
        [ HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.readLater
            , HE.onChecked (HE.input OnReadLater)
            ]
          , HH.text "Read later"
          ]

        , HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.private
            , HE.onChecked (HE.input OnPrivate)
            ]
          , HH.text "Private"
          ]
        ]
        <>
        [ HH.button
          [ PH.class_ "primary"
          , HE.onClick (HE.input (Save <<< toEvent))
          ]
          [ HH.text "Save" ]
        ]
        <>
       ([ HH.button
          [ PH.class_ "danger"
          , HE.onClick (HE.input (Delete <<< toEvent))
          ]
          [ HH.text "Delete" ]
        ] # guard (isJust s.time))
      ]

    eval :: Query m ~> DSL m
    eval q = case q of
      Init k -> k <$ do
        s <- H.get
        r <- H.lift $ getOptions { url = Just s.url }
                    # postsGet s.config.authToken

        unwrapResponse r \posts ->
          case uncons posts of
            Nothing ->
              H.modify_ (_{ status = Info "New bookmark" })

            Just { head } -> do
              let t = either identity identity (formatDateTime "MMM DD, YYYY" head.time)
              H.modify_ (_{ status    = Info ("First bookmarked " <> t)
                          , title     = head.description
                          , desc      = head.extended
                          , tags      = head.tags
                          , readLater = head.toread
                          , private   = not head.shared
                          , time      = Just head.time })

      -- The options page has changed the configuration
      Recv (Tuple c _) k -> k <$ do
        H.modify_ (_{ config = c })

        -- Reload details, perhaps with a different API token
        _ <- eval (Init k)

        -- Don't change flags if bookmark was already saved
        whenM (H.gets (isNothing <<< _.time)) $
          H.modify_ (_{ readLater = c.defaults.readLater
                      , private   = c.defaults.private })

      -- User interaction events
      Save e k -> k <$ do
        noBubble e
        H.modify_ (_{ status = Info "Saving..." })

        _ <- H.query TagSlot (H.action TI.Blur)
        s <- H.get
        r <- H.lift $ addOptions
                    { extended  = Just s.desc
                    , tags      = Just s.tags
                    , replace   = Just true
                    , shared    = Just (not s.private)
                    , toread    = Just s.readLater }
                    # postsAdd s.config.authToken s.url s.title

        unwrapResponse r \_ -> do
          H.liftEffect closePopup
          -- now <- map extract (H.liftEffect nowDateTime)
          -- H.modify_ (_{ status = Success "Saved", time = Just now })

      Delete e k -> k <$ do
        noBubble e
        H.modify_ (_{ status = Info "Deleting..." })

        s <- H.get
        r <- H.lift $ postsDelete s.config.authToken s.url

        unwrapResponse r \_ -> do
          H.liftEffect closePopup
          -- H.modify_ (_{ status = Success "Deleted", time = Nothing })

      OnBlur k -> pure k
      OnFocus k -> k <$ H.query TagSlot (H.action TI.Focus)
      OnUrl x k -> k <$ H.modify_ (_{ url = x })
      OnDesc x k -> k <$ H.modify_ (_{ desc = x })
      OnTitle x k -> k <$ H.modify_ (_{ title = x })
      OnPrivate x k -> k <$ H.modify_ (_{ private = x })
      OnReadLater x k -> k <$ H.modify_ (_{ readLater = x })

      FromTagInput tags k -> k <$ do
        toText <- H.gets _.config.tags.textValue
        H.modify_ (_{ tags = map toText tags })


unwrapResponse
  :: forall m a
   . Either Error a
  -> (a -> DSL m Unit)
  -> DSL m Unit
unwrapResponse (Right a) f = f a
unwrapResponse (Left e) _ =
  case e of
    JsonError msg  -> H.modify_ (_{ status = Error ("JSON: "  <> msg) })
    UserError msg  -> H.modify_ (_{ status = Error ("Server: " <> msg) })
    HttpError 401  -> H.modify_ (_{ status = Error "Incorrect API token" })
    HttpError code -> H.modify_ (_{ status = Error ("HTTP " <> show code) })


noBubble :: forall m. MonadAff m => Event -> DSL m Unit
noBubble = H.liftEffect <<< preventDefault

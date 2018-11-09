module Pinboard.UI.Popup.Multi
  ( component
  , Input
  , Output
  , Query(OnBlur, OnFocus)
  , State
  , Status
  , Tab
  ) where

import Prelude
import WebExtensions.Tabs.Tab         as CT
import Effect.Aff.Class               (class MonadAff)
import Effect.Aff.Jax.Class           (class MonadJax)
import Web.Event.Event                (preventDefault)
import Web.UIEvent.MouseEvent         (MouseEvent, toEvent)
import Data.Array                     (all, any, mapWithIndex, modifyAt)
import Data.Either                    (Either(..))
import Data.Filterable                (filterMap, maybeBool)
import Data.Maybe                     (Maybe(..))
import Data.String                    (Pattern(..), indexOf)
import Data.TraversableWithIndex      (forWithIndex)
import Data.Tuple                     (Tuple(..))
import Halogen                        as H
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Pinboard.API                   (Error(..), postsAdd, addOptions)
import Pinboard.Config                (Config, Tag)
import Pinboard.UI.Component.TagInput as TI
import Pinboard.UI.Internal.HTML      as PH

-------------------------------------------------------------------------------

type State m =
  { tabs      :: Array Tab
  , tags      :: Array String
  , readLater :: Boolean
  , private   :: Boolean
  , replace   :: Boolean
  , chosen    :: Array Boolean
  , config    :: Config m }

type Tab =
  { url     :: String
  , title   :: String
  , favIcon :: Maybe String
  , chosen  :: Boolean
  , status  :: Status }

data Status
  = Idle
  | Waiting
  | Success
  | Error String

derive instance eqStatus :: Eq Status

data Query m k
  = Save MouseEvent k
  | Recv (Input m) k
  | OnBlur k
  | OnFocus k
  | OnTitle Int String k
  | OnCheck Int Boolean k
  | OnReadLater Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | FromTagInput (TI.Output Tag) k

type Input m = Tuple (Config m) (Array CT.Tab)
type Output  = Void

type HTML m = H.ParentHTML (Query m) (TI.Query Tag m) Slot m
type DSL m  = H.ParentDSL (State m) (Query m) (TI.Query Tag m) Slot Output m

data Slot = TagSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

-------------------------------------------------------------------------------

component
  :: forall m
   . MonadAff m
  => MonadJax m
  => H.Component HH.HTML (Query m) (Input m) Output m
component =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: \i -> Just (Recv i unit) }
  where
    initialState :: Input m -> State m
    initialState (Tuple config tabs) =
      { tabs:       filterMap op tabs
      , tags:       []
      , readLater:  config.defaults.readLater
      , private:    config.defaults.private
      , replace:    config.defaults.replace
      , chosen:     true <$ tabs
      , config }
      where
        op t = { url: _, title: _, favIcon: _, chosen: _, status: Idle }
               <$> (maybeBool ok =<< CT.url t)
               <*> CT.title t
               <*> pure (CT.favIconUrl t)
               <*> pure true

        -- Ignore tabs for chrome://, about:, etc
        ok u = any (\s -> Pattern s `indexOf` u == Just 0)
          [ "http:"
          , "https:"
          , "javascript:"
          , "mailto:"
          , "ftp:"
          , "file:"
          , "feed:" ]

    render :: State m -> HTML m
    render s =
      HH.form
      [ HP.id_ "multi" ]
      [ HH.div
        [ PH.class_ "status light" ]
        [ HH.text "Bookmark multiple tabs" ]

      , HH.div
        [ PH.class_ "upper" ]
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

        , HH.label
          [ PH.class_ "checkbox" ]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.replace
            , HE.onChecked (HE.input OnReplace)
            ]
          , HH.text "Replace"
          ]

        , HH.button
          [ PH.class_ "primary"
          , HP.disabled (not (enableSave s))
          , HE.onClick (HE.input Save)
          ]
          [ HH.text "Save" ]

        , HH.label
          [ PH.class_ "text" ]
          [ HH.slot TagSlot TI.component
              (Tuple s.config.tags (map s.config.tags.parse s.tags))
              (HE.input FromTagInput)
          ]
        ]

      , HH.ul
        [ PH.class_ "tabs" ]
        $ flip mapWithIndex s.tabs \n tab ->
          HH.li
          [ PH.class_ case tab.status of
                        Idle -> "idle"
                        _    -> "" ]
          [ HH.label_
            [ case tab.status of
                Waiting -> HH.img [ PH.class_ "status", HP.src "busy.svg" ]
                Success -> HH.img [ PH.class_ "status", HP.src "bookmark.svg" ]
                Error x -> HH.img [ PH.class_ "status", HP.src "error.svg" ]
                Idle ->
                  HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked tab.chosen
                  , HE.onChecked (HE.input (OnCheck n))
                  ]

            , case tab.favIcon of
                Nothing  -> HH.text ""
                Just ""  -> HH.text ""
                Just url -> HH.img [ PH.class_ "favicon", HP.src url ]

            , case tab.status of
                Idle ->
                  HH.input
                  [ PH.class_ "title"
                  , HP.type_ HP.InputText
                  , HP.value tab.title
                  , HP.required true
                  , HP.spellcheck false
                  , HP.autocomplete false
                  , HE.onValueInput (HE.input (OnTitle n))
                  ]
                Error x -> HH.div [ PH.class_ "title "] [ HH.text x ]
                _       -> HH.div [ PH.class_ "title "] [ HH.text tab.title ]

            , HH.div [ PH.class_ "url" ] [ HH.text tab.url ]
            ]
          ]
      ]

    eval :: Query m ~> DSL m
    eval q = case q of
      -- The options page has changed the configuration
      Recv (Tuple c _) k -> k <$ do
        H.modify_ (\s -> s { config    = c
                          , tabs      = map (_{ status = Idle }) s.tabs
                          , readLater = c.defaults.readLater
                          , private   = c.defaults.private
                          , replace   = c.defaults.replace })

      OnBlur k -> pure k

      OnFocus k -> k <$ do
        H.query TagSlot (H.action TI.Focus)

      OnTitle n value k -> k <$ do
        H.modify_ (updateTab n (_{ title = value }))

      OnCheck n value k -> k <$ do
        H.modify_ (updateTab n (_{ chosen = value }))

      OnReadLater value k -> k <$ do
        H.modify_ (_{ readLater = value })

      OnPrivate value k -> k <$ do
        H.modify_ (_{ private = value })

      OnReplace value k -> k <$ do
        H.modify_ (_{ replace = value })

      FromTagInput tags k -> k <$ do
        toText <- H.gets _.config.tags.textValue
        H.modify_ (_{ tags = map toText tags })

      Save e k -> k <$ do
        noBubble e
        _ <- H.query TagSlot (H.action TI.Blur)
        s <- H.get

        forWithIndex s.tabs \n tab ->
          when (tab.chosen && tab.status == Idle) $ unit <$ H.fork do
            H.modify_ (updateTab n (_{ status = Waiting }))

            -- NOTE: There doesn't seem to be much benefit in running
            -- these requests in parallel; otherwise H.fork could be used
            res <- H.lift $ addOptions
                            { tags    = Just s.tags
                            , replace = Just s.replace
                            , shared  = Just (not s.private)
                            , toread  = Just s.readLater }
                          # postsAdd s.config.authToken tab.url tab.title

            case res of
              Right _  -> H.modify_ (updateTab n (_{ status = Success }))
              Left err ->
                let x = case err of
                          JsonError msg -> "JSON: "  <> msg
                          UserError msg -> "Server: " <> msg
                          HttpError 401 -> "Incorrect API token"
                          HttpError code -> "HTTP " <> show code
                 in H.modify_ (updateTab n (_{ status = Error x }))


updateTab :: forall m. Int -> (Tab -> Tab) -> State m -> State m
updateTab tabIndex f s = case modifyAt tabIndex f s.tabs of
  Nothing -> s
  Just ts -> s { tabs = ts }


enableSave :: forall m. State m -> Boolean
enableSave s =
  all (\x -> x.status /= Waiting) s.tabs &&
  any (\x -> x.chosen && paused x.status) s.tabs
  where
    paused Idle      = true
    paused (Error _) = true
    paused _         = false

noBubble
  :: forall m
   . MonadAff m
  => MouseEvent
  -> DSL m Unit
noBubble = H.liftEffect <<< preventDefault <<< toEvent

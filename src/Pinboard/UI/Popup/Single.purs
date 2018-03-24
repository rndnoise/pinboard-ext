module Pinboard.UI.Popup.Single where

import Prelude
import Data.Array                 (uncons)
import Data.DateTime              (DateTime)
import Data.Either                (Either(..), either)
import Data.Maybe                 (Maybe(..), fromMaybe, isJust)
import Data.Newtype               (class Newtype, over, unwrap, wrap)
import Data.Monoid                (guard)
import Data.Formatter.DateTime    (formatDateTime)
import Data.Tuple                 (Tuple(..))
import Control.Monad.Aff.Class    (class MonadAff)
import Control.Monad.Jax.Class    (class MonadJax)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Eff.Now      (NOW, nowDateTime)
import Control.Comonad            (extract)
import Network.HTTP.Affjax        (AJAX)
import DOM                        (DOM)
import DOM.Event.Event            as E
import DOM.Event.Types            as ET
import Halogen                    as H
import Halogen.HTML               as HH
import Halogen.HTML.Events        as HE
import Halogen.HTML.Properties    as HP

import Chrome.Tabs.Tab          (Tab, title, url) as CT
import Control.Monad.Aff.AVar   (AVAR)

import Pinboard.Config                (Config)
import Pinboard.UI.Component.TagInput as TI
import Pinboard.UI.Internal.HTML      (class_)
import Pinboard.API
  ( Post
  , Error(..)
  , postsGet
  , postsAdd
  , postsDelete
  , addOptions
  , getOptions )

-------------------------------------------------------------------------------

newtype State i m = State
  { title       :: String
  , url         :: String
  , desc        :: String
  , tags        :: Array String
  , readLater   :: Boolean
  , private     :: Boolean
  , time        :: Maybe DateTime
  , config      :: Config i m
  , status      :: Status }

derive instance newtypeState :: Newtype (State i m) _

data Status
  = Error String
  | Normal String
  | Success String

data Query i m k
  = Init k
  | Recv (Input i m) k
  | OnUrl String k
  | OnTitle String k
  | OnDesc String k
  | OnReadLater Boolean k
  | OnPrivate Boolean k
  | Save ET.MouseEvent k
  | Delete ET.MouseEvent k
  | ApiPostGet (Either Error (Array Post)) k
  | ApiPostAdd (Either Error Unit) k
  | ApiPostDelete (Either Error Unit) k
  | FromTagInput (TI.Output i) k

type Input i m = Tuple (Config i m) (Maybe CT.Tab)
type Output    = Void

data Slot = TagSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML i m = H.ParentHTML (Query i m) (TI.Query i) Slot m
type DSL i m  = H.ParentDSL (State i m) (Query i m) (TI.Query i) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall i e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM, now :: NOW | e) m
  => MonadJax m
  => Eq i
  => H.Component HH.HTML (Query i m) (Input i m) Output m
component =
  H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver:    const Nothing
  , finalizer:   Nothing
  , initializer: Just (H.action Init) }
  where
    initialState :: Input i m -> State i m
    initialState (Tuple config tab) =
      State
      { title:      fromMaybe "" (CT.title =<< tab)
      , url:        fromMaybe "" (CT.url   =<< tab)
      , desc:       ""
      , tags:       []
      , readLater:  config.defaults.readLater
      , private:    config.defaults.private
      , time:       Nothing
      , status:     Normal ""
      , config }

    render :: State i m -> HTML i m
    render (State s) =
      HH.form [HP.id_ "single"]
      [ renderStatus s.status
      , HH.div [class_ "urgh"] $
        [ HH.label [class_ "text"]
          [ HH.text "URL:"
          , HH.input
            [ HP.type_ HP.InputUrl
            , HP.value s.url
            , HE.onValueInput (HE.input OnUrl) ] ]

        , HH.label [class_ "text"]
          [ HH.text "Title:"
          , HH.input
            [ HP.type_ HP.InputText
            , HP.value s.title
            , HE.onValueInput (HE.input OnTitle) ] ]

        , HH.label [class_ "select"]
          [ HH.text "Tags:"
          , HH.slot TagSlot (TI.component s.config.tags) unit (HE.input FromTagInput) ]

        , HH.label [class_ "textarea"]
          [ HH.text "Description:"
          , HH.textarea
            [ HP.value s.desc
            , HE.onValueInput (HE.input OnDesc) ] ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.readLater
            , HE.onChecked (HE.input OnReadLater) ]
          , HH.text "Read later" ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked s.private
            , HE.onChecked (HE.input OnPrivate) ]
          , HH.text "Private" ] ]
        <>
        [ HH.button [class_ "primary", HE.onClick (HE.input Save)]
          [ HH.text "Save" ] ]
        <>
        guard (isJust s.time)
        [ HH.button [class_ "danger", HE.onClick (HE.input Delete)]
          [ HH.text "Delete" ] ] ]

      where
        renderStatus (Error x) = HH.div [class_ "status danger"] [ HH.text x ]
        renderStatus (Normal x) = HH.div [class_ "status light"] [ HH.text x ]
        renderStatus (Success x) = HH.div [class_ "status success"] [ HH.text x ]

    eval :: Query i m ~> DSL i m
    eval q = case q of
      Init k -> k <$ do
        H.modify (message "Checking...")

        State s <- H.get
        res     <- lift $ flip runReaderT s.config.authToken
                            (postsGet (getOptions { url = Just s.url }))
        eval (ApiPostGet res k)

      Recv (Tuple config _) k -> k <$ do
        H.modify (over State (_ { config = config }))

      -- user interaction events
      Save e k -> k <$ do
        noBubble e
        H.modify (message "Saving...")

        State s <- H.get
        res     <- lift $ flip runReaderT s.config.authToken
                            (postsAdd s.url s.title (addOptions
                              { extended  = Just s.desc
                              , tags      = Just s.tags
                              , replace   = Just true
                              , shared    = Just false
                              , toread    = Just s.readLater }))
        eval (ApiPostAdd res k)

      Delete e k -> k <$ do
        noBubble e
        H.modify (message "Deleting...")

        State s <- H.get
        res     <- lift (runReaderT (postsDelete s.url) s.config.authToken)
        eval (ApiPostDelete res k)

      ApiPostGet res k -> k <$ unwrapResponse res \ps -> do
        case uncons ps of
          Nothing ->
            H.modify (message "New bookmark")

          Just {head,tail} -> do
            State s <- H.get
            _       <- H.query TagSlot $ H.action (TI.SetChosen (map s.config.tags.parse head.tags))

            let fmt = either id id <<< formatDateTime "MMM DD, YYYY"
            H.modify (message ("First bookmarked " <> fmt head.time))
            H.modify (state (_ { title     = head.description
                               , desc      = head.extended
                               , tags      = head.tags
                               , readLater = head.toread
                               , private   = not head.shared
                               , time      = Just head.time }))

      ApiPostAdd res k -> k <$ unwrapResponse res \_ -> do
        now <- extract <$> H.liftEff nowDateTime
        H.modify (success "Saved" <<< state (_ { time = Just now }))

      ApiPostDelete res k -> k <$ unwrapResponse res \_ -> do
        H.modify (success "Deleted" <<< state (_ { time = Nothing }))

      OnUrl x k ->       k <$ H.modify (state (_ { url = x }))
      OnDesc x k ->      k <$ H.modify (state (_ { desc = x }))
      OnTitle x k ->     k <$ H.modify (state (_ { title = x }))
      OnPrivate x k ->   k <$ H.modify (state (_ { private = x }))
      OnReadLater x k -> k <$ H.modify (state (_ { readLater = x }))

      FromTagInput o k -> k <$ do
        State s <- H.get

        case o of
          TI.OnChosen xs ->
            H.modify (state (_ { tags = map s.config.tags.textValue xs }))


unwrapResponse :: forall i m a. Either Error a -> (a -> DSL i m Unit) -> DSL i m Unit
unwrapResponse (Right a) f = f a
unwrapResponse (Left e) _  =
  case e of
       DecodeError msg -> H.modify (error ("JSON: "   <> msg))
       ServerError msg -> H.modify (error ("Server: " <> msg))


noBubble
  :: forall e i m
   . MonadAff (dom :: DOM | e) m
  => ET.MouseEvent
  -> DSL i m Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.mouseEventToEvent


state :: forall a i m. Newtype (State i m) a => (a -> a) -> (State i m -> State i m)
state f = wrap <<< f <<< unwrap


message :: forall i m. String -> (State i m -> State i m)
message s = state (_ { status = Normal s })


error :: forall i m. String -> (State i m -> State i m)
error s = state (_ { status = Error s })


success :: forall i m. String -> (State i m -> State i m)
success s = state (_ { status = Success s })

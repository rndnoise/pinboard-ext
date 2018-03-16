module Pinboard.UI.Popup.Single where

import Prelude
import Data.Array               (uncons)
import Data.Maybe               (Maybe(..), fromMaybe, isJust)
import Data.DateTime            (DateTime)
import Data.Either              (Either(..), either)
import Data.Newtype             (class Newtype, wrap, unwrap)
import Data.Monoid              (guard)
import Data.Formatter.DateTime  (formatDateTime)
import Control.Monad.Aff.Class  (class MonadAff)
import Control.Monad.Eff.Now    (NOW, nowDateTime)
import Control.Comonad          (extract)
import Network.HTTP.Affjax      (AJAX)
import DOM                      (DOM)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP

import Chrome.Tabs.Tab          (Tab, title, url) as CT
import Control.Monad.Aff.AVar   (AVAR)

import Pinboard.API                   (Post, Error(..), postsGet, postsAdd, postsDelete, addOptions, getOptions)
import Pinboard.UI.Internal.HTML      (class_)
import Pinboard.UI.Component.TagInput as TI

-------------------------------------------------------------------------------

newtype State = State
  { title       :: String
  , url         :: String
  , desc        :: String
  , tags        :: Array String
  , toRead      :: Boolean
  , private     :: Boolean
  , time        :: Maybe DateTime
  , status      :: Status }

derive instance newtypeState :: Newtype State _

data Status
  = Error String
  | Normal String
  | Success String

data Query i k
  = Init k
  | OnUrl String k
  | OnTitle String k
  | OnDesc String k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | Save ET.MouseEvent k
  | Delete ET.MouseEvent k
  | ApiPostGet (Either Error (Array Post)) k
  | ApiPostAdd (Either Error Unit) k
  | ApiPostDelete (Either Error Unit) k
  | FromTagInput (TI.Output i) k

type Input  = Maybe CT.Tab
type Output = Void

data Slot = TagSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML i m = H.ParentHTML (Query i) (TI.Query i) Slot m
type DSL i m  = H.ParentDSL State (Query i) (TI.Query i) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall i e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM, now :: NOW | e) m
  => Eq i
  => TI.Config i m
  -> (i -> String)
  -> H.Component HH.HTML (Query i) Input Output m
component cfg encodeTag =
  H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver:    const Nothing
  , finalizer:   Nothing
  , initializer: Just (H.action Init) }
  where
    initialState :: Input -> State
    initialState t = State
      { title       : fromMaybe "" (CT.title =<< t)
      , url         : fromMaybe "" (CT.url   =<< t)
      , desc        : ""
      , tags        : []
      , toRead      : false
      , private     : true
      , time        : Nothing
      , status      : Normal "" }

    render :: State -> HTML i m
    render (State s) =
      HH.form [HP.id_ "single"]
      [ renderStatus s.status
      , HH.div [class_ "urgh"] $
        [ HH.label [class_ "text"]
          [ HH.text "URL:"
          , HH.input
            [ HP.id_ "url"
            , HP.type_ HP.InputUrl
            , HP.value s.url
            , HE.onValueInput (HE.input OnUrl) ] ]

        , HH.label [class_ "text"]
          [ HH.text "Title:"
          , HH.input
            [ HP.id_ "title"
            , HP.type_ HP.InputText
            , HP.value s.title
            , HE.onValueInput (HE.input OnTitle) ] ]

        , HH.label [class_ "select"]
          [ HH.text "Tags:"
          , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagInput) ]

        , HH.label [class_ "textarea"]
          [ HH.text "Description:"
          , HH.textarea
            [ HP.id_ "desc"
            , HP.value s.desc
            , HE.onValueInput (HE.input OnDesc) ] ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.id_ "toread"
            , HP.type_ HP.InputCheckbox
            , HP.checked s.toRead
            , HE.onChecked (HE.input OnToRead) ]
          , HH.text "Read later" ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.id_ "private"
            , HP.type_ HP.InputCheckbox
            , HP.checked s.private
            , HE.onChecked (HE.input OnPrivate) ]
          , HH.text "Private" ] ]
        <>
        guard (isJust s.time)
        [ HH.button [class_ "danger", HE.onClick (HE.input Delete)]
          [ HH.text "Delete" ] ]

        <>
        [ HH.button [class_ "primary", HE.onClick (HE.input Save)]
          [ HH.text "Save" ] ] ]

      where
        renderStatus (Error x) = HH.div [class_ "status danger"] [ HH.text x ]
        renderStatus (Normal x) = HH.div [class_ "status light"] [ HH.text x ]
        renderStatus (Success x) = HH.div [class_ "status success"] [ HH.text x ]

    eval :: Query i ~> DSL i m
    eval q = case q of
      Init k -> k <$ do
        H.modify (message "Checking...")
        url <- H.gets (_.url <<< unwrap)
        res <- H.liftAff $ postsGet (getOptions { url = Just url })
        eval (ApiPostGet res k)

      -- user interaction events
      Save e k -> k <$ do
        noBubble e
        H.modify (message "Saving...")

        s   <- H.gets unwrap
        res <- H.liftAff $ postsAdd s.url s.title (addOptions
                                    { extended = Just s.desc
                                    , tags     = Just s.tags
                                    , replace  = Just true
                                    , shared   = Just false
                                    , toRead   = Just s.toRead })
        eval (ApiPostAdd res k)

      Delete e k -> k <$ do
        noBubble e
        H.modify (message "Deleting...")

        url <- H.gets (_.url <<< unwrap)
        res <- H.liftAff $ postsDelete url
        eval (ApiPostDelete res k)

      ApiPostGet res k -> k <$ unwrapResponse res \ps -> do
        case uncons ps of
          Nothing ->
            H.modify (message "New bookmark")

          Just {head,tail} -> do
            _ <- H.query TagSlot $ H.action (TI.SetChosen (map (cfg.parse) head.tags))

            let fmt = either id id <<< formatDateTime "MMM DD, YYYY"
            H.modify (message ("First bookmarked " <> fmt head.time))
            H.modify (state (_ { title   = head.description
                               , desc    = head.extended
                               , tags    = head.tags
                               , toRead  = head.toRead
                               , private = not head.shared
                               , time    = Just head.time }))

      ApiPostAdd res k -> k <$ unwrapResponse res \_ -> do
        now <- extract <$> H.liftEff nowDateTime
        H.modify (success "Saved" <<< state (_ { time = Just now }))

      ApiPostDelete res k -> k <$ unwrapResponse res \_ -> do
        H.modify (success "Deleted" <<< state (_ { time = Nothing }))

      OnUrl x k ->     k <$ H.modify (state (_ { url = x }))
      OnDesc x k ->    k <$ H.modify (state (_ { desc = x }))
      OnTitle x k ->   k <$ H.modify (state (_ { title = x }))
      OnToRead x k ->  k <$ H.modify (state (_ { toRead = x }))
      OnPrivate x k -> k <$ H.modify (state (_ { private = x }))

      FromTagInput o k -> k <$
        case o of
          TI.OnChosen x -> H.modify (state (_ { tags = map encodeTag x }))


unwrapResponse :: forall i m a. Either Error a -> (a -> DSL i m Unit) -> DSL i m Unit
unwrapResponse (Right a) f = f a
unwrapResponse (Left e) _  =
  case e of
       DecodeError msg  -> H.modify (error ("Decode error " <> msg))
       ServerError code -> H.modify (error ("Server error " <> show code))

noBubble
  :: forall e i m
   . MonadAff (dom :: DOM | e) m
  => ET.MouseEvent
  -> DSL i m Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.mouseEventToEvent


state :: forall a. Newtype State a => (a -> a) -> (State -> State)
state f = wrap <<< f <<< unwrap


message :: String -> (State -> State)
message s = state (_ { status = Normal s })


error :: String -> (State -> State)
error s = state (_ { status = Error s })


success :: String -> (State -> State)
success s = state (_ { status = Success s })

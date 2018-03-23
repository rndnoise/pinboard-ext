module Pinboard.UI.Popup.Multi where

import Prelude
import Data.Array                 (all, mapWithIndex, modifyAt)
import Data.Either                (Either(..))
import Data.Maybe                 (Maybe(..), fromMaybe)
import Data.Newtype               (class Newtype, over, unwrap)
import Data.Filterable            (filterMap)
import Data.TraversableWithIndex  (forWithIndex)
import Control.Monad.Aff.Class    (class MonadAff)
import Control.Monad.Jax.Class    (class MonadJax)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Trans.Class  (lift)
import Network.HTTP.Affjax        (AJAX)
import DOM                        (DOM)
import DOM.Event.Event            as E
import DOM.Event.Types            as ET
import Halogen                    as H
import Halogen.Aff                as HA
import Halogen.HTML               as HH
import Halogen.HTML.Events        as HE
import Halogen.HTML.Properties    as HP

import Chrome.Tabs.Tab                as CT
import Pinboard.API                   (Error(..), postsAdd, addOptions)
import Pinboard.UI.Internal.HTML      (class_)
import Pinboard.UI.Component.TagInput as TI
import Pinboard.Config                as CF

-------------------------------------------------------------------------------

newtype State = State
  { tabs    :: Array Tab
  , tags    :: Array String
  , toRead  :: Boolean
  , private :: Boolean
  , replace :: Boolean
  , chosen  :: Array Boolean }

type Tab =
  { url     :: String
  , title   :: String
  , favIcon :: Maybe String
  , chosen  :: Boolean
  , status  :: Status }

derive instance newtypeState :: Newtype State _

data Status
  = Idle
  | Waiting
  | Success
  | Error String

derive instance eqStatus :: Eq Status

data Query i k
  = Save ET.MouseEvent k
  | OnTitle Int String k
  | OnCheck Int Boolean k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | ApiPostAdd Int (Either Error Unit) k
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
   . MonadAff (HA.HalogenEffects (ajax :: AJAX | e)) m
  => MonadJax m
  => Eq i
  => CF.Config i m
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
      { tabs:     filterMap op ts
      , tags:     []
      , toRead:   false
      , private:  true
      , replace:  true
      , chosen:   true <$ ts }
      where
        op t = { url: _, title: _, favIcon: _, chosen: _, status: Idle }
               <$> CT.url t
               <*> CT.title t
               <*> pure (CT.favIconUrl t)
               <*> pure true

    render :: State -> HTML i m
    render (State s) =
      HH.form [HP.id_ "multi", class_ "multi"]
      [ HH.div [class_ "status light"] [ HH.text "TODO" ]
      , HH.div [class_ "upper"]
        [ HH.label [class_ "select"]
          [ HH.text "Tags:"
          , HH.slot TagSlot (TI.component cfg.tags) unit (HE.input FromTagWidget) ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked true
            , HE.onChecked (HE.input OnToRead) ]
          , HH.text "Read later" ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked true
            , HE.onChecked (HE.input OnPrivate) ]
          , HH.text "Private" ]

        , HH.label [class_ "checkbox"]
          [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked true
            , HE.onChecked (HE.input OnReplace) ]
          , HH.text "Replace" ]

        , HH.button
          [ class_ "primary"
          , HP.disabled (all (\x -> not x.chosen || x.status /= Idle) s.tabs)
          , HE.onClick (HE.input Save) ]
          [ HH.text "Save" ] ]

      , HH.ul [class_ "tabs"] $ flip mapWithIndex s.tabs \n t ->
          HH.li [class_ case t.status of
                             Idle -> "idle"
                             _    -> ""]
            [ HH.label_
              [ case t.status of
                     Idle ->
                       HH.input
                       [ HP.type_ HP.InputCheckbox
                       , HP.checked t.chosen
                       , HE.onChecked (HE.input (OnCheck n)) ]
                     Waiting -> HH.img [class_ "status", HP.src "img/three.svg"]
                     Success -> HH.img [class_ "status", HP.src "img/bookmark.svg"]
                     Error x -> HH.img [class_ "status", HP.src "img/issue.svg"]
              , HH.img [class_ "favicon", HP.src (fromMaybe "" t.favIcon)]
              , case t.status of
                     Idle ->
                       HH.input
                       [ class_ "title"
                       , HP.type_ HP.InputText
                       , HP.value t.title
                       , HE.onValueInput (HE.input (OnTitle n)) ]
                     Error x -> HH.div [class_ "title"] [ HH.text x ]
                     _       -> HH.div [class_ "title"] [ HH.text t.title ]
              , HH.div [class_ "url"] [ HH.text t.url ] ] ] ]

    eval :: Query i ~> DSL i m
    eval q = case q of
      Save e k -> k <$ do
        noBubble e
        s <- H.gets unwrap

        forWithIndex s.tabs \n t ->
          when (t.chosen && t.status == Idle) $ unit <$ H.fork do
            H.modify (updateTab n (_ { status = Waiting }))

            res <- lift $ runReaderT (postsAdd t.url t.title (addOptions
                    { tags    = Just s.tags
                    , replace = Just s.replace
                    , shared  = Just (not s.private)
                    , toRead  = Just s.toRead })) cfg.authToken

            eval (ApiPostAdd n res k)

      OnTitle n value k -> k <$ do
        H.modify (updateTab n (_ { title = value }))

      OnCheck n value k -> k <$ do
        H.modify (updateTab n (_ { chosen = value }))

      OnToRead value k -> k <$ do
        H.modify (over State (_ { toRead = value }))

      OnPrivate value k -> k <$ do
        H.modify (over State (_ { private = value }))

      OnReplace value k -> k <$ do
        H.modify (over State (_ { replace = value }))

      ApiPostAdd n res k -> k <$ do
        case res of
             Right x -> H.modify (updateTab n (_ { status = Success }))
             Left x  -> let msg = case x of
                                       ServerError m -> "Server: " <> m
                                       DecodeError m -> "JSON: "   <> m
                         in H.modify (updateTab n (_ { status = Error msg }))

      FromTagWidget o k -> k <$
        case o of
          TI.OnChosen xs -> do
            H.modify (over State (_ { tags = map cfg.tags.textValue xs }))


updateTab :: Int -> (Tab -> Tab) -> State -> State
updateTab n f (State s) = case modifyAt n f s.tabs of
  Nothing -> State s
  Just ts -> State (s { tabs = ts })


noBubble
  :: forall e i m
   . MonadAff (dom :: DOM | e) m
  => ET.MouseEvent
  -> DSL i m Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.mouseEventToEvent

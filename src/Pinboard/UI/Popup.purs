module Pinboard.UI.Popup where

import Prelude
import Data.Array               (head, uncons)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Tuple               (fst)
import Data.DateTime            (DateTime)
import Data.Either              (Either(..), either)
import Data.Newtype             (class Newtype, wrap, unwrap)
import Data.Time.Duration       (Milliseconds(..))
import Data.Formatter.DateTime  (formatDateTime)
import Data.Traversable         (traverse)
import Control.Monad.Eff        (Eff)
import Control.Monad.Aff        (Aff)
import Control.Monad.Aff.Class  (class MonadAff)
import Network.HTTP.Affjax      (AJAX)
import DOM.HTML.HTMLElement     (focus) as H
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP
import Halogen.VDom.Driver      as HV
import Halogen.Aff              as HA

import Chrome.FFI               (CHROME)
import Chrome.Tabs              as CT
import Chrome.Tabs.Tab          as CT
import DOM                      (DOM)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import Control.Monad.Aff.AVar   (AVAR)

import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Complete     as CC
import Pinboard.API             (Post, Suggestions, Error(..),
                                  postsGet, postsAdd, addOptions, getOptions)


-- | This is executed when the user clicks the Pinboard toolbar icon
main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true, active = Just true })
  _    <- HV.runUI component (head tabs) body
  pure unit


-- |
cfg :: forall m. Monad m => TI.Config String m
cfg =
  { parse:      id
  , render:     HH.text
  , showDelay:  Milliseconds 150.0
  , hideDelay:  Milliseconds 150.0
  , suggest:    let f = CC.commonSubsequences CC.corpus
                 in \xs x -> pure (map fst (f x)) }

-------------------------------------------------------------------------------

newtype State = State
  { title       :: String
  , url         :: String
  , desc        :: String
  , tags        :: Array String
  , toRead      :: Boolean
  , private     :: Boolean
  , time        :: Maybe DateTime
  , status      :: Maybe Status }

derive instance newtypeState :: Newtype State _

data Status
  = Error String
  | Message String

data Query k
  = Init k
  | Exit k
  | OnUrl String k
  | OnTitle String k
  | OnDesc String k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | Save ET.MouseEvent k
  | ApiPostGet (Either Error (Array Post)) k
  | ApiPostSuggest (Either Error Suggestions) k
  | ApiPostAdd (Either Error Unit) k
  | FromTagWidget (TI.Output String) k

type Input = Maybe CT.Tab
  -- Maybe (Record (url :: Maybe String, title :: Maybe String))

type Output = Void

data Slot = TagSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query (TI.Query String) Slot m
type DSL m  = H.ParentDSL State Query (TI.Query String) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR, ajax :: AJAX | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.lifecycleParentComponent            -- ComponentSpec h s f i o m
  { initialState                        -- i -> s
  , render                              -- s -> h Void (f Unit)
  , eval                                -- f ~> (ComponentDSL s f o m)
  , receiver                            -- i -> Maybe (f Unit)
  , initializer:  Just (H.action Init)  -- Maybe (f Unit)
  , finalizer:    Just (H.action Exit) }-- Maybe (f Unit)
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
      , status      : Nothing }

    render :: State -> HTML m
    render (State s) =
      HH.div
        [ class_ "main" ]
        [ fromMaybe (HH.text "OK") (map renderStatus s.status)
        , HH.form_
            [ HH.label
                [ HP.for "url"
                , class_ "text" ]
                [ HH.text "URL:"
                , HH.input
                    [ HP.id_ "url"
                    , HP.type_ HP.InputUrl
                    , HP.value s.url
                    , HE.onValueInput (HE.input OnUrl) ] ]

            , HH.label
                [ HP.for "title"
                , class_ "text" ]
                [ HH.text "Title:"
                , HH.input
                    [ HP.id_ "title"
                    , HP.type_ HP.InputText
                    , HP.value s.title
                    , HE.onValueInput (HE.input OnTitle) ] ]

            , HH.label
                [ HP.for "tags"
                , class_ "select" ]
                [ HH.text "Tags:"
                , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagWidget) ]

            , HH.label
                [ HP.for "desc"
                , class_ "textarea" ]
                [ HH.text "Description:"
                , HH.textarea
                    [ HP.id_ "desc"
                    , HP.value s.desc
                    , HE.onValueInput (HE.input OnDesc) ] ]

            , HH.label
                [ HP.for "toread"
                , class_ "checkbox" ]
                [ HH.input
                    [ HP.id_ "toread"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked s.toRead
                    , HE.onChecked (HE.input OnToRead) ]
                , HH.text "Read later" ]

            , HH.label
                [ HP.for "private"
                , class_ "checkbox" ]
                [ HH.input
                    [ HP.id_ "private"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked s.private
                    , HE.onChecked (HE.input OnPrivate) ]
                , HH.text "Private" ]

            , HH.button
                [ HE.onClick (HE.input Save) ]
                [ HH.text "Save" ]
            ]
        ]
      where
        renderStatus (Error s)   = HH.div [ class_ "error"  ] [ HH.text s ]
        renderStatus (Message s) = HH.div [ class_ "status" ] [ HH.text s ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k -> k <$ do
        H.modify (message "Checking...")
        -- TODO remove #anchor
        url <- H.gets (_.url <<< unwrap)
        res <- H.liftAff $ postsGet (getOptions { url = Just url })
        eval (ApiPostGet res k)

      Exit k -> pure k

      -- user interaction events
      Save e k -> k <$ do
        noBubble e
        H.modify (message "Posting...")

        s   <- H.gets unwrap
        res <- H.liftAff $ postsAdd s.url s.title (addOptions
                                    { extended = Just s.desc
                                    , tags     = Just s.tags
                                    , replace  = Just true
                                    , shared   = Just false
                                    , toRead   = Just s.toRead })
        eval (ApiPostAdd res k)

      ApiPostGet res k -> k <$ do
        case res of
          Left (DecodeError msg) ->
            H.modify (error ("Decode error " <> msg))

          Left (ServerError code) ->
            H.modify (error ("Error code " <> show code))

          Right ps ->
            case uncons ps of
              Nothing ->
                H.modify (message "New bookmark")

              Just {head,tail} -> do
                _ <- H.query TagSlot $ H.action (TI.SetChosen head.tags)

                let fmt = either id id <<< formatDateTime "MMM DD, YYYY"
                H.modify (message ("First bookmarked " <> fmt head.time))
                H.modify (state (_ { title   = head.description
                                   , desc    = head.extended
                                   , tags    = head.tags
                                   , toRead  = head.toRead
                                   , private = not head.shared
                                   , time    = Just head.time }))

      ApiPostAdd res k -> k <$ do
        case res of
          Left (DecodeError msg) ->
            H.modify (error ("Decode error " <> msg))

          Left (ServerError code) ->
            H.modify (error ("Error code " <> show code))

          Right _ ->
            H.modify (message "Saved")

      ApiPostSuggest res k ->
        pure k

      OnUrl x k ->     k <$ H.modify (state (_ { url = x }))
      OnTitle x k ->   k <$ H.modify (state (_ { title = x }))
      OnDesc x k ->    k <$ H.modify (state (_ { desc = x }))
      OnToRead x k ->  k <$ H.modify (state (_ { toRead = x }))
      OnPrivate x k -> k <$ H.modify (state (_ { private = x }))

      FromTagWidget o k -> k <$
        case o of
          TI.OnChosen x -> H.modify (state (_ { tags = x }))

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing


noBubble
  :: forall e m
   . MonadAff (dom :: DOM | e) m
  => ET.MouseEvent
  -> DSL m Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.mouseEventToEvent


state :: forall a. Newtype State a => (a -> a) -> (State -> State)
state f = wrap <<< f <<< unwrap


message :: forall m. String -> (State -> State)
message s = state (_ { status = Just (Message s) })


error :: forall m. String -> (State -> State)
error s = state (_ { status = Just (Error s) })


class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ s = HP.class_ (H.ClassName s)

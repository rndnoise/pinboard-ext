module Pinboard.UI.Popup where

import Prelude
import Data.Array               (head, uncons, elem, filter)
import Data.Maybe               (Maybe(..), fromMaybe, isJust)
import Data.Tuple               (Tuple(..), fst, snd)
import Data.DateTime            (DateTime)
import Data.Either              (Either(..), either)
import Data.List                (List(..), toUnfoldable)
import Data.Newtype             (class Newtype, wrap, unwrap)
import Data.Monoid              (guard)
import Data.Sequence            (head) as S
import Data.Time.Duration       (Milliseconds(..))
import Data.Formatter.DateTime  (formatDateTime)
import Control.Monad.Aff.Class  (class MonadAff)
import Control.Monad.Eff        (Eff)
import Control.Monad.Eff.Now    (NOW, nowDateTime)
import Control.Comonad          (extract)
import Network.HTTP.Affjax      (AJAX)
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP
import Halogen.VDom.Driver      as HV
import Halogen.Aff              as HA

import Chrome.FFI               (CHROME)
import Chrome.Tabs              (query, queryOptions) as CT
import Chrome.Tabs.Tab          (Tab, title, url) as CT
import DOM                      (DOM)
import DOM.Event.Event          as E
import DOM.Event.Types          as ET
import Control.Monad.Aff.AVar   (AVAR)

import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Complete     as CC
import Pinboard.API             (Post, Error(..),
                                  postsGet, postsAdd, postsDelete, addOptions, getOptions)


-- | This is executed when the user clicks the Pinboard toolbar icon
main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME, now :: NOW)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true, active = Just true })
  _    <- HV.runUI component (head tabs) body
  pure unit


-- | This needs to be defined outside `cfg` because we don't
-- want to impose the Monad m constraint; accessing cfg.parse
-- causes "no instance found" since m is ambiguous.
cfgParse :: String -> Tuple String CC.Result
cfgParse s = Tuple s Nil

cfg :: forall m. Monad m => TI.Config (Tuple String CC.Result) m
cfg =
  { parse:        cfgParse
  , renderChoice: HH.text <<< fst
  , renderOption: HH.span_ <<< toUnfoldable <<< map fmt <<< snd
  , showDelay:    Milliseconds 150.0
  , hideDelay:    Milliseconds 150.0
  , suggest:      let f = CC.commonSubsequences CC.corpus
                   in \xs x -> pure (map fix (filter (dup xs) (f x))) }
  where
    dup xs (Tuple s _) = not (s `elem` (map fst xs))
    fix (Tuple s rs) = Tuple s (fromMaybe Nil (S.head rs))
    fmt (CC.M s) = HH.span [class_ "matched"] [HH.text s]
    fmt (CC.U s) = HH.span [class_ "unmatch"] [HH.text s]

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
  | Normal String
  | Success String

data Query k
  = Init k
  | Exit k
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
  | FromTagWidget (TI.Output (Tuple String CC.Result)) k

type Input = Maybe CT.Tab
  -- Maybe (Record (url :: Maybe String, title :: Maybe String))

type Output = Void

data Slot = TagSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query (TI.Query (Tuple String CC.Result)) Slot m
type DSL m  = H.ParentDSL State Query (TI.Query (Tuple String CC.Result)) Slot Output m

-------------------------------------------------------------------------------

component
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR, ajax :: AJAX, now :: NOW | e) m
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
      HH.div_
      [ fromMaybe (HH.text "") (map renderStatus s.status)
      , HH.div [ class_ "main" ]
        [ HH.form_ $
          [ HH.label [ HP.for "url", class_ "text" ]
            [ HH.text "URL:"
            , HH.input
              [ HP.id_ "url"
              , HP.type_ HP.InputUrl
              , HP.value s.url
              , HE.onValueInput (HE.input OnUrl) ]
            ]

          , HH.label [ HP.for "title", class_ "text" ]
            [ HH.text "Title:"
            , HH.input
              [ HP.id_ "title"
              , HP.type_ HP.InputText
              , HP.value s.title
              , HE.onValueInput (HE.input OnTitle) ]
            ]

          , HH.label [ HP.for "tags", class_ "select" ]
            [ HH.text "Tags:"
            , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagWidget) ]

          , HH.label [ HP.for "desc", class_ "textarea" ]
            [ HH.text "Description:"
            , HH.textarea
              [ HP.id_ "desc"
              , HP.value s.desc
              , HE.onValueInput (HE.input OnDesc) ]
            ]

          , HH.label [ HP.for "toread", class_ "checkbox" ]
            [ HH.input
              [ HP.id_ "toread"
              , HP.type_ HP.InputCheckbox
              , HP.checked s.toRead
              , HE.onChecked (HE.input OnToRead) ]
            , HH.text "Read later" ]

          , HH.label [ HP.for "private", class_ "checkbox" ]
            [ HH.input
              [ HP.id_ "private"
              , HP.type_ HP.InputCheckbox
              , HP.checked s.private
              , HE.onChecked (HE.input OnPrivate) ]
            , HH.text "Private" ]
          ]
          <>
          guard (isJust s.time)
          [ HH.button [ class_ "danger", HE.onClick (HE.input Delete) ]
            [ HH.text "Delete" ]
          ]
          <>
          [ HH.button [ class_ "primary", HE.onClick (HE.input Save) ]
            [ HH.text "Save" ]
          ]
        ]
      ]
      where
        renderStatus (Error x) = HH.div [ class_ "status danger"  ] [ HH.text x ]
        renderStatus (Normal x) = HH.div [ class_ "status light" ] [ HH.text x ]
        renderStatus (Success x) = HH.div [ class_ "status success" ] [ HH.text x ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k -> k <$ do
        H.modify (message "Checking...")
        url <- H.gets (_.url <<< unwrap)
        res <- H.liftAff $ postsGet (getOptions { url = Just url })
        eval (ApiPostGet res k)

      Exit k -> pure k

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
            _ <- H.query TagSlot $ H.action (TI.SetChosen (map (cfgParse) head.tags))

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
      OnTitle x k ->   k <$ H.modify (state (_ { title = x }))
      OnDesc x k ->    k <$ H.modify (state (_ { desc = x }))
      OnToRead x k ->  k <$ H.modify (state (_ { toRead = x }))
      OnPrivate x k -> k <$ H.modify (state (_ { private = x }))

      FromTagWidget o k -> k <$
        case o of
          TI.OnChosen x -> H.modify (state (_ { tags = map fst x }))

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing


unwrapResponse :: forall m a. Either Error a -> (a -> DSL m Unit) -> DSL m Unit
unwrapResponse (Right a) f = f a
unwrapResponse (Left e) _  =
  case e of
       DecodeError msg  -> H.modify (error ("Decode error " <> msg))
       ServerError code -> H.modify (error ("Server error " <> show code))

noBubble
  :: forall e m
   . MonadAff (dom :: DOM | e) m
  => ET.MouseEvent
  -> DSL m Unit
noBubble = H.liftEff <<< E.preventDefault <<< ET.mouseEventToEvent


state :: forall a. Newtype State a => (a -> a) -> (State -> State)
state f = wrap <<< f <<< unwrap


message :: String -> (State -> State)
message s = state (_ { status = Just (Normal s) })


error :: String -> (State -> State)
error s = state (_ { status = Just (Error s) })


success :: String -> (State -> State)
success s = state (_ { status = Just (Success s) })


class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ s = HP.class_ (H.ClassName s)

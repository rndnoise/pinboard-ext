module Pinboard.UI.Popup where

import Prelude
import Data.Array               (head)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Tuple               (fst)
import Data.DateTime            (DateTime)
import Data.Time.Duration       (Milliseconds(..))
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
import Control.Monad.Aff.AVar   (AVAR)
import Pinboard.API             (Post, postsGet, getOptions)
import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Complete     as CC

-- | This is executed when the user clicks the Pinboard toolbar icon
-- |
main :: Eff (HA.HalogenEffects (chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true, active = Just true })
  _    <- HV.runUI component (head tabs) body
  pure unit

cfg :: forall m. Monad m => TI.Config String m
cfg =
  { parse:      id
  , render:     HH.text
  , showDelay:  Milliseconds 150.0
  , hideDelay:  Milliseconds 150.0
  , suggest:    let f = CC.commonSubsequences CC.corpus
                 in \xs x -> pure (map fst (f x)) }

-------------------------------------------------------------------------------

type State =
  { title       :: String
  , url         :: String
  , desc        :: String
  , tags        :: Array String
  , toRead      :: Boolean
  , suggested   :: Maybe (Array String)
  , recommended :: Maybe (Array String)
  , others      :: Maybe Number
  , time        :: Maybe DateTime }

data Query k
  = Init k
  | Exit k
  | OnUrl String k
  | OnTitle String k
  | OnDesc String k
  | OnToRead Boolean k
  | Save k
  | FromTagWidget (TI.Output String) k

type Input = Maybe CT.Tab

type Output = Void

data Slot = TagSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query TI.Query Slot m
type DSL m  = H.ParentDSL State Query TI.Query Slot Output m

-------------------------------------------------------------------------------

component
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
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
    initialState t =
      { title       : fromMaybe "" (CT.title =<< t)
      , url         : fromMaybe "" (CT.url   =<< t)
      , desc        : ""
      , tags        : []
      , toRead      : false
      , suggested   : Nothing
      , recommended : Nothing
      , others      : Nothing
      , time        : Nothing }

    render :: State -> HTML m
    render s =
      HH.div
        [ HP.class_ (HH.ClassName "main") ]
        [ HH.h1_ [ HH.text "New/Edit Bookmark" ]
        , HH.form_
            [ HH.label
                [ HP.for "url"
                , HP.class_ (HH.ClassName "text") ]
                [ HH.text "URL:"
                , HH.input
                    [ HP.id_ "url"
                    , HP.type_ HP.InputUrl
                    , HP.value s.url
                    , HE.onValueInput (HE.input OnUrl) ] ]

            , HH.label
                [ HP.for "title"
                , HP.class_ (HH.ClassName "text") ]
                [ HH.text "Title:"
                , HH.input
                    [ HP.id_ "title"
                    , HP.type_ HP.InputText
                    , HP.value s.title
                    , HE.onValueInput (HE.input OnTitle) ] ]

            , HH.label
                [ HP.for "tags"
                , HP.class_ (HH.ClassName "select") ]
                [ HH.text "Tags:"
                , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagWidget) ]

            , HH.label
                [ HP.for "desc"
                , HP.class_ (HH.ClassName "textarea") ]
                [ HH.text "Description:"
                , HH.textarea
                    [ HP.id_ "desc"
                    , HP.value s.desc
                    , HE.onValueInput (HE.input OnDesc) ] ]

            , HH.label
                [ HP.for "later"
                , HP.class_ (HH.ClassName "checkbox") ]
                [ HH.input
                    [ HP.id_ "later"
                    , HP.type_ HP.InputCheckbox
                    , HE.onChecked (HE.input OnToRead) ]
                , HH.text "Read later" ]

            , HH.button
                [ HE.onClick (HE.input_ Save) ]
                [ HH.text "Save" ]
            ]
        ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k            -> pure k
      Exit k            -> pure k
      Save k            -> pure k
      OnUrl x k         -> k <$ H.modify (_ { url = x })
      OnTitle x k       -> k <$ H.modify (_ { title = x })
      OnDesc x k        -> k <$ H.modify (_ { desc = x })
      OnToRead x k      -> k <$ H.modify (_ { toRead = x })
      FromTagWidget o k -> k <$
        case o of
             TI.OnChosen x -> H.modify (_ { tags = x })

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

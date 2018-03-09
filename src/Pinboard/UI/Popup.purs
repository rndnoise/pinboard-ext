module Pinboard.UI.Popup where

import Prelude
import Data.Maybe               (Maybe(..))
import Data.DateTime            (DateTime)
import Control.Monad.Eff        (Eff)
import Control.Monad.Aff        (Aff)
import Control.Monad.Aff.Class  (class MonadAff)
import Network.HTTP.Affjax      (AJAX)
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP
import Halogen.VDom.Driver      as HV
import Halogen.Aff              as HA

import Chrome.FFI               (CHROME)
import DOM                      (DOM)
import Control.Monad.Aff.AVar   (AVAR)
import Pinboard.API             (Post, postsGet, getOptions)
import Pinboard.UI.TagInput     as T

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
  = Set Boolean k
  | Get (Boolean -> k)
  | OnTag T.Output k

type Input = Unit

type Output = Void

data Slot = TagSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query T.Query Slot m
type DSL m  = H.ParentDSL State Query T.Query Slot Output m

-- | This is executed when the user clicks the Pinboard toolbar icon
-- |
main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  _    <- HV.runUI component unit body
  pure unit

component
  :: forall e m
   . MonadAff (dom :: DOM, avar :: AVAR | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.parentComponent               -- ComponentSpec h s f i o m
  { initialState                  -- i -> s
  , render                        -- s -> h Void (f Unit)
  , eval                          -- f ~> (ComponentDSL s f o m)
  , receiver:     const Nothing } -- i -> Maybe (f Unit)
  where
    initialState :: Input -> State
    initialState _ =
      { title       : ""
      , url         : ""
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
                    , HP.value s.url ] ]

            , HH.label
                [ HP.for "title"
                , HP.class_ (HH.ClassName "text") ]
                [ HH.text "Title:"
                , HH.input
                    [ HP.id_ "title"
                    , HP.type_ HP.InputText
                    , HP.value s.title ] ]

            , HH.label
                [ HP.for "desc"
                , HP.class_ (HH.ClassName "textarea") ]
                [ HH.text "Description:"
                , HH.textarea
                    [ HP.id_ "desc"
                    , HP.value s.desc ] ]

            , HH.label
                [ HP.for "tags"
                , HP.class_ (HH.ClassName "select") ]
                [ HH.text "Tags:"
                , HH.slot TagSlot T.component unit (HE.input OnTag) ]

            , HH.label
                [ HP.for "later"
                , HP.class_ (HH.ClassName "checkbox") ]
                [ HH.input
                    [ HP.id_ "later"
                    , HP.type_ HP.InputCheckbox ]
                , HH.text "Read later" ]

            , HH.button [] [ HH.text "Save" ]
            , HH.button [] [ HH.text "Cancel" ]
            ]
        ]

    eval :: Query ~> DSL m
    eval q = case q of
      Get k -> pure (k true)
      Set v k -> pure k
      OnTag o k -> pure k

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

module Pinboard.UI.Popup.Multi where

import Prelude
import Data.Array               (elem, filter, mapWithIndex)
import Data.List                (List(..), toUnfoldable)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Newtype             (class Newtype)
import Data.Sequence            as S
import Data.Time.Duration       (Milliseconds(..))
import Data.Tuple               (Tuple(..), fst, snd)
import Control.Monad.Eff        (Eff)
import Control.Monad.Aff.AVar   (AVAR)
import Control.Monad.Aff.Class  (class MonadAff)
import Network.HTTP.Affjax      (AJAX)
import DOM                      (DOM)
import DOM.Event.Types          as ET
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP
import Halogen.VDom.Driver      as HV
import Halogen.Aff              as HA

import Chrome.FFI               (CHROME)
import Chrome.Tabs              (query, queryOptions) as CT
import Chrome.Tabs.Tab          (Tab, title, url, favIconUrl) as CT

import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Complete     as CC

main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  _    <- HV.runUI component tabs body
  pure unit

-------------------------------------------------------------------------------

newtype State = State
  { tabs :: Array CT.Tab
  }

derive instance newtypeState :: Newtype State _

data Query k
  = Init k
  | Exit k
  | Save ET.MouseEvent k
  | OnTitle Int String k
  | OnChoose Int Boolean k
  | OnToRead Boolean k
  | OnPrivate Boolean k
  | OnReplace Boolean k
  | FromTagWidget (TI.Output (Tuple String CC.Result)) k

type Input = Array CT.Tab
type Output = Void

data Slot = TagSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query (TI.Query (Tuple String CC.Result)) Slot m
type DSL m  = H.ParentDSL State Query (TI.Query (Tuple String CC.Result)) Slot Output m

-------------------------------------------------------------------------------

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
    fmt (CC.M s) = HH.span [ class_ "matched "] [ HH.text s ]
    fmt (CC.U s) = HH.span [ class_ "unmatch "] [ HH.text s ]

-------------------------------------------------------------------------------

component
  :: forall e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver
  , initializer:  Just (H.action Init)
  , finalizer:    Just (H.action Exit) }
  where
    initialState :: Input -> State
    initialState ts = State { tabs: ts }

    render :: State -> HTML m
    render (State s) =
      HH.div_
      [ HH.text ""
      , HH.div [ class_ "multi" ]
        [ HH.form_

          [ HH.div [ class_ "top" ]
              [ HH.label [ HP.for "tags", class_ "select" ]
                [ HH.text "Tags:"
                , HH.slot TagSlot (TI.component cfg) unit (HE.input FromTagWidget) ]

              , HH.label [ HP.for "toread", class_ "checkbox" ]
                [ HH.input
                  [ HP.id_ "toread"
                  , HP.type_ HP.InputCheckbox
                  , HP.checked true
                  , HE.onChecked (HE.input OnToRead) ]
                , HH.text "Read later" ]
              , HH.label [ HP.for "private", class_ "checkbox" ]
                [ HH.input
                  [ HP.id_ "private"
                  , HP.type_ HP.InputCheckbox
                  , HP.checked true
                  , HE.onChecked (HE.input OnPrivate) ]
                , HH.text "Private" ]
              , HH.label [ HP.for "replace", class_ "checkbox" ]
                [ HH.input
                  [ HP.id_ "replace"
                  , HP.type_ HP.InputCheckbox
                  , HP.checked true
                  , HE.onChecked (HE.input OnReplace) ]
                , HH.text "Replace" ]

              , HH.button [ class_ "primary", HE.onClick (HE.input Save) ]
                [ HH.text "Save" ] ]

          , HH.ul_ $ flip mapWithIndex s.tabs \n tab ->
              HH.li_
                [ HH.label [ HP.for ("tab" <> show n) ]
                  [ HH.input
                    [ HP.id_ ("tab" <> show n)
                    , HP.type_ HP.InputCheckbox
                    , HP.checked true
                    , HE.onChecked (HE.input (OnChoose n)) ]
                  , HH.img [ HP.src (fromMaybe "" (CT.favIconUrl tab)) ]
                  , HH.input
                    [ class_ "title"
                    , HP.type_ HP.InputText
                    , HP.value (fromMaybe "" (CT.title tab))
                    , HE.onValueInput (HE.input (OnTitle n)) ]
                  , HH.div [ class_ "url"   ] [ HH.text (fromMaybe "" (CT.url tab)) ] ] ]
          ]
        ]
      ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k -> pure k
      Exit k -> pure k
      Save e k -> pure k
      OnTitle n s k -> pure k
      OnChoose n b k -> pure k
      OnToRead b k -> pure k
      OnPrivate b k -> pure k
      OnReplace b k -> pure k
      FromTagWidget o k -> k <$
        case o of
          TI.OnChosen x -> pure unit

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing


class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ s = HP.class_ (H.ClassName s)

module Pinboard.UI.Popup where

import Prelude
import Data.Array               (elem, filter, find)
import Data.List                (List(Nil), toUnfoldable)
import Data.Either              (Either(..))
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Sequence            (head)
import Data.Tuple               (Tuple(..), fst, snd)
import Data.Time.Duration       (Milliseconds(..))
import Data.Newtype             (class Newtype, over)
import DOM                      (DOM)
import Control.Monad.Aff.Class  (class MonadAff)
import Control.Monad.Eff        (Eff)
import Control.Monad.Eff.Now    (NOW)
import Control.Monad.Aff.AVar   (AVAR)
import Network.HTTP.Affjax      (AJAX)
import Halogen                  as H
import Halogen.Aff              as HA
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.VDom.Driver      as HV

import Halogen.Component.ChildPath    as CP
import Data.Either.Nested             (Either2)
import Data.Functor.Coproduct.Nested  (Coproduct2)

import Chrome.FFI               (CHROME)
import Chrome.Tabs              (Tab, query, queryOptions) as CT
import Chrome.Tabs.Tab          (active) as CT
import Pinboard.UI.HTML         (class_, classes)
import Pinboard.UI.Icons        as PI
import Pinboard.UI.Complete     as CC
import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Popup.Multi  as PM
import Pinboard.UI.Popup.Single as PS

-------------------------------------------------------------------------------

cfg :: forall m. Applicative m => TI.Config (Tuple String CC.Result) m
cfg =
  { parse:        flip Tuple Nil
  , renderChoice: HH.text <<< fst
  , renderOption: HH.span_ <<< toUnfoldable <<< map fmt <<< snd
  , renderText:   fst
  , showDelay:    Milliseconds 150.0
  , hideDelay:    Milliseconds 150.0
  , suggest:      let f = CC.commonSubsequences CC.corpus
                   in \xs x -> pure (map fix (filter (dup xs) (f x))) }
  where
    dup xs (Tuple s _) = not (s `elem` (map fst xs))
    fix (Tuple s rs) = Tuple s (fromMaybe Nil (head rs))
    fmt (CC.M s) = HH.span [ class_ "matched "] [HH.text s]
    fmt (CC.U s) = HH.span [ class_ "unmatch "] [HH.text s]

main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME, now :: NOW)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  _    <- HV.runUI component tabs body
  pure unit

-------------------------------------------------------------------------------

data Query k
  = OnClickMulti k
  | OnClickSingle k

newtype State = State
  { oneTab  :: Maybe CT.Tab
  , allTabs :: Array CT.Tab
  , active  :: Slot }

derive instance stateNewtype :: Newtype State _

type Input  = Array CT.Tab
type Output = Void

type Tag = Tuple String CC.Result

type Slot   = Either2    Unit           Unit
type Query' = Coproduct2 (PM.Query Tag) (PS.Query Tag)

type HTML m = H.ParentHTML Query Query' Slot m
type DSL m  = H.ParentDSL State Query Query' Slot Output m

component
  :: forall e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM, now :: NOW | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input -> State
    initialState tabs = State
      { active:   single
      , oneTab:   find CT.active tabs
      , allTabs:  tabs }

    render :: State -> HTML m
    render (State s) =
      HH.div_
      [ HH.div
          [ classes [ toggle multi, "multi-icon" ]
          , HE.onClick (HE.input_ OnClickMulti) ]
          [ PI.multi [] ]
      , HH.div
          [ classes [ toggle single, "single-icon" ]
          , HE.onClick (HE.input_ OnClickSingle) ]
          [ PI.single [] ]
      , HH.div
          [ classes [ toggle multi, "multi" ] ]
          [ HH.slot' CP.cp1 unit (PM.component cfg) s.allTabs absurd ]
      , HH.div
          [ classes [ toggle single, "single" ] ]
          [ HH.slot' CP.cp2 unit (PS.component cfg) s.oneTab absurd ] ]
      where
        toggle x = if s.active == x then "active" else "dormant"

    eval :: Query ~> DSL m
    eval q = case q of
      OnClickMulti k  -> k <$ H.modify (over State (_ { active = multi  }))
      OnClickSingle k -> k <$ H.modify (over State (_ { active = single }))

    multi    = Left unit
    single   = Right (Left unit)

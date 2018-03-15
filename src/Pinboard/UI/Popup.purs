module Pinboard.UI.Popup where

import Prelude
import Data.Array               (elem, filter)
import Data.List                (List(Nil), toUnfoldable)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Sequence            (head)
import Data.Tuple               (Tuple(..), fst, snd)
import Data.Time.Duration       (Milliseconds(..))
import DOM                      (DOM)
import Control.Monad.Eff        (Eff)
import Control.Monad.Aff.AVar   (AVAR)
import Network.HTTP.Affjax      (AJAX)
import Halogen                  as H
import Halogen.Aff              as HA
import Halogen.HTML             as HH
import Halogen.VDom.Driver      as HV

import Chrome.FFI               (CHROME)
import Chrome.Tabs              as CT
import Pinboard.UI.HTML         (class_)
import Pinboard.UI.Icons        as PI
import Pinboard.UI.Complete     as CC
import Pinboard.UI.TagInput     as TI
import Pinboard.UI.Popup.Multi  as PM
import Pinboard.UI.Popup.Single as PS

cfg :: TI.Config (Tuple String CC.Result) m
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

main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  _    <- HV.runUI component tabs body
  pure unit

-------------------------------------------------------------------------------

data Query k
  = Init k
  | Exit k

type Input  = Array CT.Tab
type Output = Void

data Slot = Single | Multi
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m  = H.ParentDSL State Query ChildQuery Slot Output m

component
  :: forall e m
   . MonadAff (ajax :: AJAX, avar :: AVAR, dom :: DOM | e) m
  => H.Component HH.HTML Query Input Output m
component =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input -> State
    initialState ts = unit

    render :: State -> HTML m
    render (State s) =
      HH.div_
      [ PI.multi
      , PI.single
      , HH.slot TagSlot (PS.component cfg) unit (HE.input FromSingleTab)
      , HH.slot TagSlot (PM.component cfg) unit (HE.input FromMultiTab) ]

    eval :: Query ~> DSL m
    eval q = case q of
      Init k -> pure k
      Exit k -> pure k

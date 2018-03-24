module Pinboard.UI.Popup where

import Prelude
import Data.Array                     (find)
import Data.Either                    (Either(..))
import Data.Maybe                     (Maybe(..))
import Data.Newtype                   (class Newtype, over)
import Data.Tuple                     (Tuple(..))
import Control.Monad.Aff.Class        (class MonadAff)
import Control.Monad.Jax.Class        (class MonadJax)
import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Now          (NOW)
import Network.HTTP.Affjax            (AJAX)
import Halogen                        as H
import Halogen.Aff                    as HA
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Halogen.VDom.Driver            as HV

import Halogen.Component.ChildPath    as CP
import Data.Either.Nested             (Either2)
import Data.Functor.Coproduct.Nested  (Coproduct2)

import Chrome.FFI                     (CHROME)
import Chrome.Tabs                    (Tab, query, queryOptions) as CT
import Chrome.Tabs.Tab                (active) as CT

import Pinboard.Config                (Config, loadConfig)
import Pinboard.UI.Internal.HTML      (classes)
import Pinboard.UI.Popup.Multi        as PM
import Pinboard.UI.Popup.Single       as PS

-------------------------------------------------------------------------------

main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME, now :: NOW)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  conf <- loadConfig
  _    <- HV.runUI component (Tuple conf tabs) body
  pure unit

-------------------------------------------------------------------------------

data Query k
  = OnClickMulti k
  | OnClickSingle k

newtype State i m = State
  { oneTab  :: Maybe CT.Tab
  , allTabs :: Array CT.Tab
  , active  :: Slot
  , config  :: Config i m }

derive instance stateNewtype :: Newtype (State i m) _

type Input i m  = Tuple (Config i m) (Array CT.Tab)
type Output     = Void

type Slot       = Either2    Unit         Unit
type Query' i m = Coproduct2 (PM.Query i m) (PS.Query i m)

type HTML i m = H.ParentHTML Query (Query' i m) Slot m
type DSL i m  = H.ParentDSL (State i m) Query (Query' i m) Slot Output m

component
  :: forall eff i m
   . MonadAff (HA.HalogenEffects (ajax :: AJAX, now :: NOW | eff)) m
  => MonadJax m
  => Eq i
  => H.Component HH.HTML Query (Input i m) Output m
component =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input i m -> State i m
    initialState (Tuple config allTabs) =
      State
      { active: single
      , oneTab: find CT.active allTabs
      , allTabs
      , config }

    render :: State i m -> HTML i m
    render (State s) =
      HH.div_
      [ HH.img
          [ HP.src "img/plus-3.svg"
          , classes [ toggle multi, "multi-icon" ]
          , HE.onClick (HE.input_ OnClickMulti) ]
      , HH.img
          [ HP.src "img/plus.svg"
          , classes [ toggle single, "single-icon" ]
          , HE.onClick (HE.input_ OnClickSingle) ]
      , HH.div
          [ classes [ toggle multi, "multi" ] ]
          [ HH.slot' CP.cp1 unit PM.component (Tuple s.config s.allTabs) absurd ]
      , HH.div
          [ classes [ toggle single, "single" ] ]
          [ HH.slot' CP.cp2 unit PS.component (Tuple s.config s.oneTab) absurd ] ]
      where
        toggle x = if s.active == x then "active" else "dormant"

    eval :: Query ~> DSL i m
    eval q = case q of
      OnClickMulti k  -> k <$ H.modify (over State (_ { active = multi  }))
      OnClickSingle k -> k <$ H.modify (over State (_ { active = single }))

    multi    = Left unit
    single   = Right (Left unit)

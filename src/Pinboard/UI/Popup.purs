module Pinboard.UI.Popup
  ( main
  ) where

import Prelude
import Chrome.FFI                     (CHROME)
import Chrome.Tabs                    (Tab, query, queryOptions) as CT
import Chrome.Tabs.Tab                (active) as CT
import Control.Monad.Aff.Class        (class MonadAff)
import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Now          (NOW)
import Control.Monad.Jax.Class        (class MonadJax)
import Data.Array                     (find)
import Data.Either                    (Either(..))
import Data.Either.Nested             (Either3)
import Data.Functor.Coproduct.Nested  (Coproduct3)
import Data.Maybe                     (Maybe(..))
import Data.Tuple                     (Tuple(..))
import Halogen                        as H
import Halogen.Aff                    as HA
import Halogen.Component.ChildPath    (cp1, cp2, cp3)
import Halogen.HTML                   as HH
import Halogen.HTML.Events            as HE
import Halogen.HTML.Properties        as HP
import Halogen.VDom.Driver            as HV
import Network.HTTP.Affjax            (AJAX)
import Pinboard.Config                (Config, loadConfig)
import Pinboard.UI.Internal.HTML      as PH
import Pinboard.UI.Popup.Multi        as PM
import Pinboard.UI.Popup.Options      as PO
import Pinboard.UI.Popup.Single       as PS

-------------------------------------------------------------------------------

main :: Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME, now :: NOW)) Unit
main = HA.runHalogenAff do
  conf <- loadConfig
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  body <- HA.awaitBody
  _    <- HV.runUI component (Tuple conf tabs) body

  pure unit

-------------------------------------------------------------------------------

data Query m k
  = OnClickMulti k
  | OnClickSingle k
  | OnClickOptions k
  | FromOptions (PO.Output m) k

type State m =
  { oneTab  :: Maybe CT.Tab
    -- ^ The active browser tab when Pinboard icon was opened

  , allTabs :: Array CT.Tab
    -- ^ All browser tabs in the window when Pinboard icon was clicked

  , active  :: Slot
    -- ^ Which of options/single/multi is displayed

  , config  :: Config m }

type Input m = Tuple (Config m) (Array CT.Tab)
type Output  = Void

type Slot =
  Either3
    Unit
    Unit
    Unit

type Query' m =
  Coproduct3
    (PM.Query m)
    (PS.Query m)
    PO.Query

type HTML m = H.ParentHTML (Query m) (Query' m) Slot m
type DSL m  = H.ParentDSL (State m) (Query m) (Query' m) Slot Output m

component
  :: forall eff m
   . MonadAff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME, now :: NOW | eff)) m
  => MonadJax m
  => H.Component HH.HTML (Query m) (Input m) Output m
component =
  H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing }
  where
    initialState :: Input m -> State m
    initialState (Tuple config allTabs) =
      { active: if show config.authToken == ""
                  then optionsSlot
                  else singleSlot
      , oneTab: find CT.active allTabs
      , allTabs
      , config }

    render :: State m -> HTML m
    render s =
      HH.div_
      [ HH.img
          [ HP.src "img/single.svg"
          , PH.classes [ toggle singleSlot, "single-icon" ]
          , HE.onClick (HE.input_ OnClickSingle)
          ]

      , HH.img
          [ HP.src "img/multi.svg"
          , PH.classes [ toggle multiSlot, "multi-icon" ]
          , HE.onClick (HE.input_ OnClickMulti)
          ]

      , HH.img
          [ HP.src "img/options.svg"
          , PH.classes [ toggle optionsSlot, "options-icon" ]
          , HE.onClick (HE.input_ OnClickOptions)
          ]

      , HH.div
          [ PH.classes [ toggle singleSlot, "single" ] ]
          [ HH.slot' cp2 unit PS.component (Tuple s.config s.oneTab) absurd ]

      , HH.div
          [ PH.classes [ toggle multiSlot, "multi" ] ]
          [ HH.slot' cp1 unit PM.component (Tuple s.config s.allTabs) absurd ]

      , HH.div
          [ PH.classes [ toggle optionsSlot, "options" ] ]
          [ HH.slot' cp3 unit PO.component s.config (HE.input FromOptions) ] ]

      where
        toggle x = if s.active == x then "active" else "dormant"

    eval :: Query m ~> DSL m
    eval q = case q of
      OnClickMulti k -> k <$ H.modify (_{ active = multiSlot  })
      OnClickSingle k -> k <$ H.modify (_{ active = singleSlot })
      OnClickOptions k -> k <$ H.modify (_{ active = optionsSlot })
      FromOptions config k -> k <$ H.modify (_{ config = config })

    multiSlot   = Left unit
    singleSlot  = Right (Left unit)
    optionsSlot = Right (Right (Left unit))

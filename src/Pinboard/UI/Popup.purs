module Pinboard.UI.Popup
  ( main
  ) where

import Prelude
import WebExtensions.Tabs             (Tab, query, queryOptions) as CT
import WebExtensions.Tabs.Events      (onRemoved) as CT
import WebExtensions.Tabs.Tab         (active, id, windowId) as CT
import Effect                         (Effect)
import Effect.Aff.Class               (class MonadAff)
import Effect.Aff.Jax.Class           (class MonadJax)
import Data.Array                     (find)
import Data.Either.Nested             (Either3, in1, in2, in3)
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
import Pinboard.Config                (Config, loadConfig)
import Pinboard.UI.Internal.HTML      as PH
import Pinboard.UI.Internal.Popup     (closePopup)
import Pinboard.UI.Popup.Multi        as PM
import Pinboard.UI.Popup.Options      as PO
import Pinboard.UI.Popup.Single       as PS

-------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff do
  conf <- loadConfig
  tabs <- CT.query (CT.queryOptions { currentWindow = Just true })
  body <- HA.awaitBody
  _    <- HV.runUI component (Tuple conf tabs) body

  -- Close the popup if the active tab is closed
  H.liftEffect $ CT.onRemoved $ \tabId info -> do
    case find CT.active tabs of
      Just t | CT.windowId t == info.windowId
            && CT.id t == Just tabId -> closePopup
      _                              -> pure unit

  pure unit

-------------------------------------------------------------------------------

data Query m k
  = Init k
  | OnClickMulti k
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

type Slot = Either3 Unit Unit Unit

type Query' m =
  Coproduct3
    (PM.Query m)
    (PS.Query m)
    PO.Query

type HTML m = H.ParentHTML (Query m) (Query' m) Slot m
type DSL m  = H.ParentDSL (State m) (Query m) (Query' m) Slot Output m

component
  :: forall m
   . MonadAff m
  => MonadJax m
  => H.Component HH.HTML (Query m) (Input m) Output m
component =
  H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , finalizer:   Nothing
  , initializer: Just (H.action Init)
  , receiver:    const Nothing }
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
          [ HP.src "single.svg"
          , PH.classes [ toggle singleSlot, "single-icon" ]
          , HE.onClick (HE.input_ OnClickSingle)
          ]

      , HH.img
          [ HP.src "multi.svg"
          , PH.classes [ toggle multiSlot, "multi-icon" ]
          , HE.onClick (HE.input_ OnClickMulti)
          ]

      , HH.img
          [ HP.src "options.svg"
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
      Init k -> k <$ do
        x <- H.gets (_.active)
        case unit of
          _ | x == multiSlot -> H.query' cp1 unit (H.action PM.OnFocus)
          _ | x == singleSlot -> H.query' cp2 unit (H.action PS.OnFocus)
          _ | x == optionsSlot -> H.query' cp3 unit (H.action PO.OnFocus)
          _ -> pure Nothing

      OnClickMulti k -> k <$ do
        H.modify_ (_{ active = multiSlot  })
        swapFocus multiSlot

      OnClickSingle k -> k <$ do
        H.modify_ (_{ active = singleSlot })
        swapFocus singleSlot

      OnClickOptions k -> k <$ do
        H.modify_ (_{ active = optionsSlot })
        swapFocus optionsSlot

      FromOptions config k -> k <$ do
        H.modify_ (_{ config = config })

    multiSlot = in1 unit
    singleSlot = in2 unit
    optionsSlot = in3 unit

    swapFocus y = do
      x <- H.gets (_.active)
      _ <- case unit of
        _ | x == multiSlot -> H.query' cp1 unit (H.action PM.OnBlur)
        _ | x == singleSlot -> H.query' cp2 unit (H.action PS.OnBlur)
        _ | x == optionsSlot -> H.query' cp3 unit (H.action PO.OnBlur)
        _ -> pure Nothing

      case unit of
        _ | y == multiSlot -> H.query' cp1 unit (H.action PM.OnFocus)
        _ | y == singleSlot -> H.query' cp2 unit (H.action PS.OnFocus)
        _ | y == optionsSlot -> H.query' cp3 unit (H.action PO.OnFocus)
        _ -> pure Nothing

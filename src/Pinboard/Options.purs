module Pinboard.Options where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))

import Halogen              as H
import Halogen.HTML         as HH
import Halogen.HTML.Events  as HE
import Halogen.VDom.Driver  as HV
import Halogen.Aff          as HA

import Chrome.FFI (CHROME)

-- |
type State =
  { title :: String
  , url   :: String
  , desc  :: String
  , tags  :: Array String
  }

-- |
data Query k
  = Set Boolean k
  | Get (Boolean -> k)

-- |
type Input = Unit

-- |
type Message = Void

-- |
main :: Eff (HA.HalogenEffects (chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io   <- HV.runUI ui unit body
  pure unit

ui :: forall e. H.Component HH.HTML Query Input Message (Aff e)
ui =
  H.component                         -- ComponentSpec h s f i o m
  { initialState: const initialState  -- i -> s
  , render:       render              -- s -> h Void (f Unit)
  , eval:         eval                -- f ~> (ComponentDSL s f o m)
  , receiver:     const Nothing }     -- i -> Maybe (f Unit)
  where
    initialState :: State
    initialState =
      { title: "Where have all the insects gone?"
      , url:   "http://www.sciencemag.org/news/2017/05/where-have-all-insects-gone"
      , desc:  "Fireflies, like these in a forest in the Netherlands, have disappeared from some areas in North America and Europe where they were once abundant."
      , tags:  ["type:article", "science"] }

    render :: State -> H.ComponentHTML Query
    render s =
      HH.div_ [ HH.text "Oh." ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff e)
    eval q = case q of
      Get k ->
        pure (k true)
      Set v k -> do
        pure k

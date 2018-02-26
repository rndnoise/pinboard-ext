module Pinboard.Background where

import Prelude
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Argonaut.Core (Json, toObject)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Halogen              as H
import Halogen.HTML         as HH
import Halogen.HTML.Events  as HE
import Halogen.VDom.Driver  as HV
import Halogen.Aff          as HA
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax  as AX

import Debug.Trace as D
import Chrome.FFI (CHROME)
import Chrome.Tabs    as T
import Chrome.Windows as W

-- | This is executed when the browser starts
main :: forall eff. Eff (HA.HalogenEffects (ajax :: AJAX, chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  pure unit



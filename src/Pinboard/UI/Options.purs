module Pinboard.UI.Options
  ( main
  ) where

import Prelude
import Control.Monad.Eff    (Eff)

-- | This is executed when the config page is shown
main :: forall eff. Eff eff Unit
main = pure unit

module Pinboard.UI.Background
  ( main
  ) where

import Prelude
import Control.Monad.Eff    (Eff)

-- | This is executed when the browser starts
main :: forall eff. Eff eff Unit
main = pure unit

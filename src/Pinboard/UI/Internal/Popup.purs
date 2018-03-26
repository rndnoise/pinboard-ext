module Pinboard.UI.Internal.Popup
  ( closePopup
  ) where

import Prelude
import DOM                (DOM)
import Control.Monad.Eff  (Eff)

closePopup :: forall eff. Eff (dom :: DOM | eff) Unit
closePopup = _closePopup
foreign import _closePopup :: forall eff. Eff (dom :: DOM | eff) Unit

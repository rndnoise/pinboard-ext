module Pinboard.UI.Internal.Popup
  ( closePopup
  ) where

import Prelude
import Effect (Effect)

closePopup :: Effect Unit
closePopup = _closePopup
foreign import _closePopup :: Effect Unit

module Pinboard.UI.HTML where

import Prelude                  ((<<<), map)
import Halogen.HTML             as H
import Halogen.HTML.Properties  as HP

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< H.ClassName

classes :: forall r i. Array String -> HP.IProp ("class" :: String | r) i
classes = HP.classes <<< map H.ClassName

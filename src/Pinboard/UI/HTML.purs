module Pinboard.UI.HTML where

import Halogen.HTML             as H
import Halogen.HTML.Properties  as HP

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ s = HP.class_ (H.ClassName s)

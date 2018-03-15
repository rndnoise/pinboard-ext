module Halogen.SVG.Attributes where

import Prelude                  (Unit, (#), (<<<), map, show)
import Data.Foldable            (intercalate)
import Data.Maybe               (Maybe(..))
import Data.Newtype             (unwrap)
import Halogen.HTML.Core        as Core
import Halogen.HTML.Core        (Prop, AttrName(AttrName), ClassName, Namespace)
import Halogen.HTML.Properties  (IProp)
import Halogen.Query.InputF     (InputF)
import Unsafe.Coerce            (unsafeCoerce)


ns :: Maybe Namespace
ns = Nothing -- Just (Namespace "http://www.w3.org/2000/svg")


-- | Creates an indexed SVG attribute.
attr
  :: forall r i
   . AttrName
  -> String
  -> IProp r i
attr =
  Core.attr ns #
    (unsafeCoerce
      :: (AttrName -> String -> Prop (InputF Unit i))
      -> AttrName
      -> String
      -> IProp r i)

class_ :: forall r i. ClassName -> IProp ("class" :: String | r) i
class_ = attr (AttrName "class") <<< unwrap

viewBox :: forall r i. Int -> Int -> Int -> Int -> IProp (viewBox :: String | r) i
viewBox w x y z = attr (AttrName "viewBox") (intercalate " " (map show [w,x,y,z]))

version :: forall r i. String -> IProp (version :: String | r ) i
version = attr (AttrName "version")

width :: forall r i. Int -> IProp (width :: Int | r) i
width = attr (AttrName "width") <<< show

height :: forall r i. Int -> IProp (height :: Int | r) i
height = attr (AttrName "height") <<< show

fillRule :: forall r i. String -> IProp ("fill-rule" :: String | r) i
fillRule = attr (AttrName "fill-rule")

d :: forall r i. String -> IProp (d :: String | r) i
d = attr (AttrName "d")

strokeWidth :: forall r i. Number -> IProp (strokeWidth :: Number | r) i
strokeWidth = attr (AttrName "strokeWidth") <<< show

strokeLineJoin :: forall r i. String -> IProp (strokeLineJoin :: String | r) i
strokeLineJoin = attr (AttrName "strokeLineJoin")

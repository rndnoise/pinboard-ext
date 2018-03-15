module Halogen.SVG.Elements
  ( module Exports
  , element
  , svg
  , path
  ) where

import Prelude                  ((#))
import Unsafe.Coerce            (unsafeCoerce)
import Data.Maybe               (Maybe(Just))
import Halogen.HTML.Core        as Core
import Halogen.HTML.Core        (HTML, Prop)
import Halogen.HTML.Properties  (IProp)
import Halogen.HTML.Elements    (Node)
import Halogen.VDom             (ElemName(..), Namespace(..))
import Halogen.SVG.Indexed      as I

import Halogen.HTML.Properties 
  (IProp) as Exports

ns :: Maybe Namespace
ns = Just (Namespace "http://www.w3.org/2000/svg")

-- | Creates an SVG element that expects indexed properties.
element
  :: forall r p i
   . ElemName
  -> Array (IProp r i)
  -> Array (HTML p i)
  -> HTML p i
element =
  Core.element ns #
    (unsafeCoerce
      :: (ElemName
            -> Array (Prop i)
            -> Array (HTML p i)
            -> HTML p i)
      -> ElemName
      -> Array (IProp r i)
      -> Array (HTML p i)
      -> HTML p i)

svg :: forall p i. Node I.SVG p i
svg = element (ElemName "svg")

path :: forall p i. Node I.SVGpath p i
path = element (ElemName "path")

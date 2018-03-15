module Halogen.SVG.Indexed where

import DOM.HTML.Indexed (Interactive)

type SVG = Interactive
  ( "class" :: String
  , height :: Int
  , width :: Int
  , viewBox :: String
  , version :: String )

type SVGpath = Interactive
  ( d :: String
  , "fill-rule" :: String
  , strokeWidth :: Number
  , strokeLineJoin :: String )

module Halogen.SVG.Indexed where

type SVG =
  ( "class" :: String
  , height :: Int
  , width :: Int
  , viewBox :: String
  , version :: String )

type SVGpath =
  ( d :: String
  , "fill-rule" :: String
  , strokeWidth :: Number
  , strokeLineJoin :: String )

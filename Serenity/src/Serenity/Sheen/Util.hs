module Serenity.Sheen.Util where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

coloredRectangle :: Color -> (Float, Float) -> Picture
coloredRectangle c (width, height) = Color c $ Polygon [(0,0), (0, height), (width, height), (width, 0)]

changeAlpha :: Color -> Float -> Color
changeAlpha color alpha = let (r,g,b,_) = rgbaOfColor color in makeColor r g b alpha
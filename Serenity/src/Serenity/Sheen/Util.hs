module Serenity.Sheen.Util where

import Control.Lens
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Extent

coloredRectangle :: Color -> (Float, Float) -> Picture
coloredRectangle c (width, height) = Color c $ Polygon [(0,0), (0, height), (width, height), (width, 0)]

changeAlpha :: Color -> Float -> Color
changeAlpha color alpha = let (r,g,b,_) = rgbaOfColor color in makeColor r g b alpha

toExtent ((xmin, ymin), (xsize, ysize)) = makeExtent (ymin+ysize) ymin (xmin+xsize) xmin
fromExtent extent = ((d, b), (c-d, a-b)) where (a,b,c,d) = takeExtent extent

extentOrigin :: Simple Lens Extent (Int, Int)
extentOrigin = lens (fst.fromExtent) (\ext org -> toExtent $ _1 .~ org $ fromExtent ext)

extentSize :: Simple Lens Extent (Int, Int)
extentSize = lens (fst.fromExtent) (\ext sze -> toExtent $ _2 .~ sze $ fromExtent ext)

overMaybe getter setter f = do
	x <- use getter
	case x of
		Just x' -> setter %= f x'
		Nothing -> return ()

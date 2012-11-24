module Serenity.Game.Client.Common where

import Serenity.Game.Shared.Model.Common

-- | The size of the Gloss window
windowSize :: (Int, Int)
windowSize = (1024, 768)

-- | The view port is the area of the game world that is being viewed
-- by the client. (x, y, width, height)
type ViewPort = (Float, Float, Float, Float)

-- | A change in the view port's x and y coordinates
type ViewPortMove = (Float, Float)

-- | A change to the view port's zoom level
type ViewPortZoom = (Float, Float, Float, Float)

-- | Convert a view port location into an in-game map location
mapLocationFromView ::
	Location    -- ^ Location within the view port
	-> ViewPort -- ^ View port
	-> Size     -- ^ Size of the map
	-> Location

mapLocationFromView (x, y) (vx, vy, vw, vh) (w, h) = (mapX, mapY)
	where
		mapX = (x / ww) * vw + vx
		mapY = (y / wh) * vh + vy

		ww = fromIntegral $ fst windowSize
		wh = fromIntegral $ snd windowSize

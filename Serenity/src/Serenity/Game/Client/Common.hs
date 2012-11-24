module Serenity.Game.Client.Common where

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

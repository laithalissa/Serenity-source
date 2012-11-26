
module Serenity.Game.Server.Math
(	distance
,	unitVector
) where


distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = ( (x2-x1)**2 + (y2-y1)**2 )**0.5

unitVector :: (Float, Float) -> (Float, Float)
unitVector (x,y) = (x/magnitude, y/magnitude)
	where
--		magnitude = (x**2 + y**2)**0.5
		magnitude = distance (0,0) (x,y)


 
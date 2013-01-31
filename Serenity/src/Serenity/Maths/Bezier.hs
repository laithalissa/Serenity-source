{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.Maths.Bezier 
(	bezier
,	bezierR
,	bezierM
,	bezierRM
,	parameterizeByArcLength
,	parameterizeByArcLength'
,	arcLength
) where

import Serenity.Maths.Vector

import Data.List(unfoldr)
import Serenity.Maths.Util (rangeLimitAttainBounds)

----------------------------------- Bezier Curves ----------------------------------

-- | A Bezier curve in a given vector space
bezier :: (VectorSpace v, s ~ Scalar v) => [v] -> s -> v
  -- bezier of just one point is fixed at that point
bezier [p] t = p
  -- bezier of a list of points is a linear interpolation between
  -- beziers of the initial part of the list and the tail.
bezier ps  t = lerp (bezier (init ps) t) (bezier (tail ps) t) t

-- | A bezier curve reparameterised for constant arc length with respect to curve parameter.
bezierR :: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s, Ord s, VectorSpace s, Scalar s ~ Scalar v)
	=> [v] -> s -> v
bezierR ps = parameterizeByArcLength (bezier ps)

bezierM :: (VectorSpace v, s ~ Scalar v, Fractional v, Num s, RealFrac s) => [v] -> s -> v
bezierM pts' t' = ((beziers ++ [last beziers]) !! (fromIntegral $ toInteger f)) ((t*n)- (fromIntegral f)) where
	pts = multipleBezierPoints pts'
	beziers = map bezier pts
	n = fromIntegral $ length pts
	f = floor (n*t)
	t = rangeLimitAttainBounds 0 1 t'

	multipleBezierPoints (p1:p2:ps) = splitEvery 4 $ p1:p2:(init $ makeIntermediatePoints ps)

	makeIntermediatePoints []         = []
	makeIntermediatePoints [p]        = [p]
	makeIntermediatePoints (p1:p2:ps) = let interm = (p1+p2)*0.5 
		in p1 : interm : interm : p2 : (makeIntermediatePoints ps)

bezierRM 
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s, Ord s, VectorSpace s, Scalar s ~ Scalar v, RealFrac s, Fractional v)
	=> [v] -> s -> v
bezierRM ps = parameterizeByArcLength (bezierM ps)

------------------------- Reparameterisation by Arc Length -------------------------

-- | Estimate the arc lengths of a curve between  by breaking the curve into a number 
--   of divisions and using the euclidean distance.
arcLengths
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s)
	=> (s -> v) -- ^ Curve
	-> s        -- ^ Divisions
	-> (s, s)   -- ^ Range
	-> [s]      -- ^ Arc lengths at each division
arcLengths f divs (t0, t1) = zipWith distance (init points) (tail points) where
	divisions = map (\x -> t0 + x*(t1-t0)/divs) [0..divs]
	points = map f divisions

-- | Cumulative arc lengths.
arcLengthSums 
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s)
	=> (s -> v) -- ^ Curve
	-> s        -- ^ Divisions
	-> (s, s)   -- ^ Range
	-> [s]      -- ^ Total arc lengths at each division
arcLengthSums f divs (t0, t1) = scanl1 (+) $ arcLengths f divs (t0, t1)

-- | Total curve length
arcLength 
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s)
	=> (s -> v) -- ^ Curve
	-> s        -- ^ Divisions
	-> (s, s)   -- ^ Range
	-> s        -- ^ Length of curve
arcLength f divs (t0, t1) = last $ arcLengthSums f divs (t0, t1)

-- | Conversion between curve parameter and constant arc length parameter,
--   estimated using linear interpolation within cumulative arc length.
getCurveParameter
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s, Ord s, VectorSpace s, Scalar s ~ Scalar v)
	=> (s -> v) -- ^ Curve
	-> s        -- ^ Divisions
	-> (s, s)   -- ^ Range
	-> s        -- ^ Curve parameter for original curve
	-> s        -- ^ Curve parameter for reparameterised curve
getCurveParameter curve divs (start, end) s = findIndex lengths 0 0 (s*curveLength) where
	lengths = arcLengthSums curve divs (start,end)
	curveLength = last lengths
	step = (end-start)/divs
	findIndex [] _ _ _ = 0
	findIndex (l:ls) previous index s = if s < l 
		then lerp (index-step) index ((s-previous)/(l-previous))
		else findIndex ls l (index + step) s

-- | Convert curve to arc length parameterised curve.
parameterizeByArcLength'
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s, Ord s, VectorSpace s, Scalar s ~ Scalar v)
	=> (s -> v) -- ^ Curve
	-> s        -- ^ Divisions
	-> (s, s)   -- ^ Range
	-> (s -> v) -- ^ Reparameterised curve
parameterizeByArcLength' curve divs range s = curve $ getCurveParameter curve divs range s

-- | Convert curve to arc length parameterised curve between 0 and 1 using 100 divisions.
parameterizeByArcLength
	:: (InnerSpace v, s ~ Scalar v, Num s, Floating s, Enum s, Ord s, VectorSpace s, Scalar s ~ Scalar v)
	=> (s -> v) -- ^ Curve
	-> (s -> v) -- ^ Reparameterised curve
parameterizeByArcLength  curve s = curve $ getCurveParameter curve 100 (0,1) s

------------------------------------- Handy --------------------------------------------

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

lerplist :: RealFrac a => [a] -> a -> a
lerplist list value = if value - (fromIntegral index) < 1.0e-10
		then a
		else ((1.0 - alpha) * a) + (alpha * b)
		where
			index = fromIntegral $ toInteger $ truncate value
			alpha = value - (fromIntegral index)
			a = list !! index
			b = list !! (index + 1)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.Maths.Util where

import Data.Fixed (mod')
import GHC.Float
import Data.VectorSpace

-- | Force a value to be within bounds, attaing the bound when the value is outside.
rangeLimitAttainBounds :: Ord a 
	=> a -- ^ Minimum
	-> a -- ^ Maximum
	-> a -- ^ Unrestrained input value
	-> a -- ^ Output value in range [Minimum, Maximum]
rangeLimitAttainBounds minx maxx x = case x of
	x | x < minx -> minx
	x | x > maxx -> maxx
	x -> x

-- | Force a real value to be within bounds.
rangeLimitWrapReal :: Real a 
	=> a -- ^ Minimum
	-> a -- ^ Maximum
	-> a -- ^ Unrestrained input value
	-> a -- ^ Output value in range [Minimum, Maximum)
rangeLimitWrapReal minx maxx x = minx + (mod' (x-minx) (maxx-minx))

-- | Force an integral value to be within bounds.
rangeLimitWrapInt :: Integral a 
	=> a -- ^ Minimum
	-> a -- ^ Maximum
	-> a -- ^ Unrestrained input value
	-> a -- ^ Output value in range [Minimum, Maximum)
rangeLimitWrapInt minx maxx x = minx + (mod (x-minx) (maxx-minx))

-- | Check whether a value is within bounds.
rangeCheck :: Ord a 
	=> a -- ^ Minimum
	-> a -- ^ Maximum
	-> a -- ^ Unrestrained input value x
	-> Either a a -- ^ Right x if within range [Minimum, Maximum], else Left x
rangeCheck minx maxx x = case x of
	x | x <= minx -> Left x
	x | x >= maxx -> Left x
	x -> Right x

pDouble2Float (x,y) = (double2Float x,double2Float y)
pFloat2Double (x,y) = (float2Double x,float2Double y)

differentiate 
	:: (Floating s, Num (s, s), Ord s, InnerSpace s, Scalar s ~ s, v ~ (s,s), RealFrac s, Fractional (s,s), Enum s)
	=> ((s->v),s) -> v
differentiate (curve, s) = (curve(s+h)^-^curve(s-h))^/(2*h) where h = 0.01

data Partition = One | Two | Three

part3 :: (a -> Maybe Partition) -> [a] -> ([a],[a],[a])
part3 p xs = foldl (part3' p) ([], [], []) xs 
	where
	part3' p (a, b, c) x = case p x of
		Just One   -> (x:a, b  , c  )
		Just Two   -> (a  , x:b, c  )
		Just Three -> (a  , b  , x:c)
		_ -> (a, b, c)

-- Find the centre of a two-dimensional region
centroid :: [(Double, Double)] -> (Double, Double)
centroid ps = (sumV ps) ^/ (fromIntegral $ length ps)

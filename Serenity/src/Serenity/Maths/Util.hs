module Serenity.Maths.Util where

import Data.Fixed (mod')

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
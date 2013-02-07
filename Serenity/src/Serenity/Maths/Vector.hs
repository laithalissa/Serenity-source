{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.Maths.Vector
(	module Data.VectorSpace
,	side
,	rotate
,	angleBetween
,	distance
) where

import Data.VectorSpace

-- | Returns true or false depending on whether the second vector is closest to a right or left rotation of the second. (2d Vectors).
side (a1, a2) (b1, b2) = a1*b2-a2*b1 > 0

-- | Rotate a 2D vector by a given number of radians.
rotate theta (x,y) = (x*(cos theta) - y*(sin theta), x*(sin theta) + y*(cos theta))

-- | Angle (in radians) between two vectors.
angleBetween a b = acos $ (a <.> b) / ((magnitude a) * (magnitude b))

-- | Euclidean distance
distance a b = magnitude $ negateV a ^+^ b 
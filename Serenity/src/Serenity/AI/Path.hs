{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.AI.Path where

import Serenity.Maths.Bezier
import Serenity.Maths.Vector
import Serenity.Maths.Util

-- | Make a smooth path with a limited radius of curvature between two points and directions.
makePoints 
	:: (Floating s, Num (s, s), Ord s, InnerSpace s, Scalar s ~ s) 
	=> s                -- ^ Minimum radius of curvature at turning points
	-> ((s, s), (s, s)) -- ^ Starting location with direction
	-> ((s, s), (s, s)) -- ^ Ending location with direction
	-> [(s, s)]         -- ^ List of points to give to bezierRM
makePoints radius (startLoc, startDir) (endLoc, endDir) = [startS, forwardS, sidewaysS, sidewaysE, backE, forwardE] where
	startS    = startLoc
	forwardS  = startLoc ^+^ radius *^(normalized startDir)
	sidewaysS = forwardS ^+^ radius *^(normalized $ rotateStart startDir)
	forwardE  = endLoc   ^+^ radius *^(normalized endDir)
	backE     = endLoc   ^+^ radius *^(negateV   $ normalized endDir)
	sidewaysE = backE    ^+^ radius *^(normalized $ rotateEnd endDir)

	side' x loc dir = side dir (x-loc)
	sideStart = side' endLoc   startLoc startDir
	sideEnd   = side' startLoc endLoc   endDir

	rotateStart = rotate $ (if sideStart then 1 else -1) * (rangeLimitAttainBounds 0  (pi/2) $ angleBetween startDir $ backE - forwardS)
	rotateEnd   = rotate $ (if sideEnd   then 1 else -1) * (rangeLimitAttainBounds (pi/2) pi $ angleBetween endDir   $ forwardS - backE)

makePath :: (Floating s, Num (s, s), Ord s, InnerSpace s, Scalar s ~ s, v ~ (s,s), RealFrac s, Fractional (s,s), Enum s)
	=> s                -- ^ Minimum radius of curvature at turning points
	-> ((s, s), (s, s)) -- ^ Starting location with direction
	-> ((s, s), (s, s)) -- ^ Ending location with direction
	-> ((s -> v), s)    -- ^ Path
makePath radius start end = bezierRMLength (makePoints radius start end)
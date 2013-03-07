{-# LANGUAGE Arrows #-}

module Serenity.AI.Navigation
(	nearestPlanet
)  where

import Serenity.Model.Common
import Serenity.Model.Sector
import Serenity.Model.Wire

import Control.Lens
import Prelude hiding (id, (.))

-- | takes a sector and a location and returns the nearest planet and its distance
nearestPlanet :: BaseWire (Sector, Location) (PlanetID, Double) 
nearestPlanet = proc (sector, location) -> do
	planets <- arr sectorPlanets' -< sector
	let f = min' $ planetDistance location
	closestPlanet <- arr $ uncurry foldl1 -< (f, planets)
	id -< (closestPlanet^.planetID, planetDistance location closestPlanet)
	
min' :: (a -> Double) -> a -> a -> a
min' f a1 a2 = if (f a1) <= (f a2) then a1 else a2

planetDistance :: Location -> Planet  -> Double
planetDistance location planet = distance location (planet^.planetLocation)

distance :: Location -> Location -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2 )

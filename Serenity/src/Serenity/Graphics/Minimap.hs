{-# LANGUAGE RankNTypes #-}

module Serenity.Graphics.Minimap where

import Serenity.Model
import Serenity.Sheen
import Serenity.Maths.Util

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import GHC.Float

import Data.Map (Map)
import qualified Data.Map as Map

miniMapSize = (200,200)

scaleToFit :: (Double, Double) -> (Float, Float)
scaleToFit doubles = (pDouble2Float $ doubles) & (_1 %~ (/(fromIntegral x))) & (_2 %~ (/(fromIntegral y))) where
	(x,y) = miniMapSize

drawMinimap :: a -> Getter a Sector -> View a
drawMinimap a aSector = (initView ((0,0),miniMapSize) )
	{	_viewDepict = Just $ Pictures $ planets
	} where
	sector = a^.aSector
	planets = map picturePlanet (Map.elems $ sector^.sectorPlanets)
	spaceLanes = map (pictureSpaceLane $ a^.aSector.sectorPlanets) (sector^.sectorSpaceLanes)

picturePlanet :: Planet -> Picture
picturePlanet p = translate x y (color planetColor $ circleSolid 4) where
	(x,y) = scaleToFit (p^.planetLocation)
	planetColor = case p^.planetEcotype of
		Blue   -> green
		Desert -> yellow
		Metal  -> greyN 0.2
		Ocean  -> blue

pictureSpaceLane :: (Map Int Planet) -> SpaceLane -> Picture
pictureSpaceLane planetsMap (p1, p2) = color (dark green) $ lane where
	lane = line $ map (\p -> scaleToFit (p^.planetLocation)) planets
	planets = catMaybes $ map (\k -> planetsMap^.(at k)) [p1, p2]


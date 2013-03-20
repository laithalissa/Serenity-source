{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Minimap
(	minimap
) where

import Serenity.Model
import Serenity.Sheen
import Serenity.Maths.Util
import Serenity.Game.Client.Color

import Control.Lens
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

minimap :: a -> Getter a Game -> Int -> (Int, Int) -> View a
minimap a aGame oID miniMapSize = (initView ((0,0),miniMapSize) )
	& (viewDepict .~ (Just $ Pictures $ spaceLanes ++ planets ++ ships))
	& (viewBackground .~ (Just black))
	where
	sector = a^.aGame.gameBuilder.gbSector
	sSize = sector^.sectorSize
	planets = map (picturePlanet miniMapSize sSize) (Map.elems $ sector^.sectorPlanets)
	spaceLanes = map (pictureSpaceLane miniMapSize sSize $ sector^.sectorPlanets) (sector^.sectorSpaceLanes)
	ships = map (pictureShip miniMapSize sSize oID) (map _entityData (filter (\x -> x^.ownerID == oID) (Map.elems (a^.aGame.gameShips))))

scaleToFit :: (Int, Int) -> (Double, Double) -> (Double, Double) -> (Float, Float)
scaleToFit miniMapSize sSize doubles = (pDouble2Float $ doubles) & (_1 %~ ((/sX).(*(fromIntegral x)))) & (_2 %~ ((/sY).(*(fromIntegral y)))) where
	(x,y) = miniMapSize
	(sX,sY) = pDouble2Float sSize

picturePlanet :: (Int, Int) -> (Double, Double) -> Planet -> Picture
picturePlanet miniMapSize sSize p = translate x y (color planetColor $ circleSolid 4) where
	(x,y) = scaleToFit miniMapSize sSize (p^.planetLocation)
	planetColor = case p^.planetEcotype of
		Blue   -> dark $ dark green
		Desert -> dark orange
		Metal  -> greyN 0.5
		Ocean  -> (bright.light) blue
		Star   -> bright yellow

pictureSpaceLane :: (Int, Int) -> (Double, Double) -> (Map Int Planet) -> SpaceLane -> Picture
pictureSpaceLane miniMapSize sSize planetsMap (p1, p2) = color (dark $ dark $ dark $ dark green) $ lane where
	lane = line $ map (\p -> scaleToFit miniMapSize sSize (p^.planetLocation)) planets
	planets = catMaybes $ map (\k -> planetsMap^.(at k)) [p1, p2]

pictureShip :: (Int, Int) -> (Double, Double) -> Int -> Ship -> Picture
pictureShip miniMapSize sSize oID ship = translate x y $ rotate ((atan2 dx dy)/pi * 180) $ shipPicture where
	(x,y) = scaleToFit miniMapSize sSize $ ship^.shipLocation
	(dx,dy) = pDouble2Float $ ship^.shipDirection
	shipPicture = color (ownerIDColor oID) $ polygon [(0,0),(3,8),(6,0)]

{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Minimap where

import Serenity.Model
import Serenity.Sheen
import Serenity.Maths.Util
import Serenity.Game.Client.Color

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import GHC.Float

import Data.Map (Map)
import qualified Data.Map as Map

minimap :: a -> Getter a Game -> Int -> View a
minimap a aGame oID = (initView ((0,0),miniMapSize) )
	& (viewDepict .~ (Just $ Pictures $ spaceLanes ++ planets ++ ships))
	& (viewBackground .~ (Just black))
	where
	sector = a^.aGame.gameBuilder.gbSector
	sSize = sector^.sectorSize
	planets = map (picturePlanet sSize) (Map.elems $ sector^.sectorPlanets)
	spaceLanes = map (pictureSpaceLane sSize $ sector^.sectorPlanets) (sector^.sectorSpaceLanes)
	ships = map (pictureShip sSize oID) (map _entityData (filter (\x -> x^.ownerID == oID) (Map.elems (a^.aGame.gameShips))))

miniMapSize = (200,200)

scaleToFit :: (Double, Double) -> (Double, Double) -> (Float, Float)
scaleToFit sSize doubles = (pDouble2Float $ doubles) & (_1 %~ ((/sX).(*(fromIntegral x)))) & (_2 %~ ((/sY).(*(fromIntegral y)))) where
	(x,y) = miniMapSize
	(sX,sY) = pDouble2Float sSize

picturePlanet :: (Double, Double) -> Planet -> Picture
picturePlanet sSize p = translate x y (color planetColor $ circleSolid 4) where
	(x,y) = scaleToFit sSize (p^.planetLocation)
	planetColor = case p^.planetEcotype of
		Blue   -> dark $ dark green
		Desert -> dark yellow
		Metal  -> greyN 0.7
		Ocean  -> blue

pictureSpaceLane :: (Double, Double) -> (Map Int Planet) -> SpaceLane -> Picture
pictureSpaceLane sSize planetsMap (p1, p2) = color (dark $ dark $ dark $ dark green) $ lane where
	lane = line $ map (\p -> scaleToFit sSize (p^.planetLocation)) planets
	planets = catMaybes $ map (\k -> planetsMap^.(at k)) [p1, p2]

pictureShip :: (Double, Double) -> Int -> Ship -> Picture
pictureShip sSize oID ship = translate x y $ rotate ((atan2 dx dy)/pi * 180) $ shipPicture where
	(x,y) = scaleToFit sSize $ ship^.shipLocation
	(dx,dy) = pDouble2Float $ ship^.shipDirection
	shipPicture = color (ownerIDColor oID) $ polygon [(0,0),(3,8),(6,0)]

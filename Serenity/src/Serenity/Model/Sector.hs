{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Sector where

import Serenity.Model.Common

import Control.Lens
import Data.AdditiveGroup
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Binary
import Data.DeriveTH

type SpaceLane = (PlanetID, PlanetID)
type PlanetID = Int

data Sector = Sector
	{	_sectorName         		:: String
	,	_sectorSize         		:: (Double, Double)
	,	_sectorSpawnPoints  		:: [Location]
	,	_sectorPlanets      		:: Map PlanetID Planet
	,	_sectorSpaceLanes   		:: [SpaceLane]	
	,	_sectorSpaceLaneSpeedMultiplier :: Double
	}
	deriving (Show, Eq)

data Planet = Planet
	{	_planetID        :: PlanetID
	,	_planetName      :: String
	,	_planetEcotype   :: Ecotype
	,	_planetLocation  :: Location
	,	_planetResources :: Resources
	}
	deriving (Show, Eq)

data Ecotype = 
	  Blue
	| Desert
	| Metal
	| Ocean
	| Star
	deriving (Show, Eq)

ecotypeAssetName' :: Ecotype -> String
ecotypeAssetName' Blue   = "planet1"
ecotypeAssetName' Desert = "desert-planet"
ecotypeAssetName' Metal  = "metal-planet"
ecotypeAssetName' Ocean  = "ocean-planet"
ecotypeAssetName' Star   = "star"
ecotypeAssetName :: Getter Ecotype String
ecotypeAssetName = to ecotypeAssetName'

data Resources = Resources 
	{	_resFuel       :: Int
	,	_resMetal      :: Int
	,	_resAntimatter :: Int
	}
	deriving (Show, Eq)

makeLenses ''Resources
makeLenses ''Planet
makeLenses ''Sector
derive makeBinary ''Ecotype

makeRes a b c = Resources {_resFuel = a, _resMetal = b, _resAntimatter = c}

instance AdditiveGroup Resources where
	zeroV = Resources {_resFuel = 0, _resMetal = 0, _resAntimatter = 0}
	a ^+^ b = Resources
		{	_resFuel       = (+) (a^.resFuel)       (b^.resFuel)
		,	_resMetal      = (+) (a^.resMetal)      (b^.resMetal)
		,	_resAntimatter = (+) (a^.resAntimatter) (b^.resAntimatter)
		}
	negateV a = Resources
		{	_resFuel       = - a ^. resFuel
		,	_resMetal      = - a ^. resMetal
		,	_resAntimatter = - a ^. resAntimatter
		}

sectorOne = Sector
	{	_sectorName        = "Sector One"
	,	_sectorSize        = (1000, 1000)
	,	_sectorSpawnPoints = [(40,40), (960,40), (40,960), (960,960)]
	,	_sectorPlanets     = Map.fromList $ zip [1..] $ zipWith (planetID .~ ) [1..]
		[	Planet 1 "Thoria"              Star   (500, 500) (makeRes 5  5  20)
		,	Planet 2 "2955 Volantis Prime" Metal  (500, 300) (makeRes 10 20 0 )
		,	Planet 3 "7200 Araetis"        Metal  (500, 700) (makeRes 10 20 0 )
		,	Planet 4 "Derida"              Desert (200, 500) (makeRes 5  5  0 )
		,	Planet 5 "Arietis"             Desert (800, 500) (makeRes 5  5  0 )
		,	Planet 6 "Castillon"           Blue   (50 , 50 ) (makeRes 10 0  0 )
		,	Planet 7 "Elden Kennett"       Blue   (950, 950) (makeRes 10 0  0 )
		,	Planet 8 "Alcantar"            Ocean  (50 , 950) (makeRes 10 0  0 )
		,	Planet 9 "New Zaldi"           Ocean  (950, 50)  (makeRes 10 0  0 )
		]
	,	_sectorSpaceLaneSpeedMultiplier = 4.0
	,	_sectorSpaceLanes = [(1,2),(1,3),(4,6),(4,8),(2,6),(2,9),(5,9),(5,7),(3,7),(3,8)]
	}

sectorTwo = Sector
	{	_sectorName        = "Sector Two"
	,	_sectorSize        = (500, 500)
	,	_sectorSpawnPoints = [(50,50), (50,450), (450,50), (450,450)]
	,	_sectorPlanets     = Map.fromList
		[	makePlanet 1 (100, 100)
		,	makePlanet 2 (400, 100)
		,	makePlanet 3 (100, 400)
		,	makePlanet 4 (400, 400)
		,	makePlanet 5 (150, 250)
		,	makePlanet 6 (250, 350)
		,	makePlanet 7 (250, 150)
		,	makePlanet 8 (350, 250)
		]
	,	_sectorSpaceLanes  = 
		[	(1,4)
		, 	(3,2)
		,	(5,6)
		,	(5,7)
		,	(8,6)
		,	(8,7)
		]
	,	_sectorSpaceLaneSpeedMultiplier = 4.0
	}

	where 
	makePlanet pid location = (pid, Planet {_planetID = pid, _planetName = "Splearth" , _planetEcotype = Blue  , _planetLocation  = location, _planetResources = res 10 10 0})


planetLocation' :: Sector -> PlanetID -> Location
planetLocation' sector planetID = (fromJust $ Map.lookup planetID (sector^.sectorPlanets))^.planetLocation

sectorPlanets' :: Sector -> [Planet]
sectorPlanets' sector = map snd $ Map.toList $ sector^.sectorPlanets

sectorPlanet' :: Sector -> PlanetID -> Planet
sectorPlanet' sector pID = foldl1 (\p1 p2 -> if p1^.planetID == pID then p1 else p2) (sectorPlanets' sector)

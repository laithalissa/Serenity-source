{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Sector where

import Control.Lens
import Data.AdditiveGroup
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Binary
import Data.DeriveTH

type SpaceLane = (PlanetID, PlanetID)
type PlanetID = Int

data Sector = Sector
	{	_sectorName         :: String
	,	_sectorSize         :: (Double, Double)
	,	_sectorSpawnPoints  :: [(Double, Double)]
	,	_sectorPlanets      :: Map PlanetID Planet
	,	_sectorSpaceLanes   :: [SpaceLane]	
	}

data Planet = Planet
	{	_planetID        :: PlanetID
	,	_planetName      :: String
	,	_planetEcotype   :: Ecotype
	,	_planetLocation  :: (Double, Double)
	,	_planetResources :: Resources
	}

data Ecotype = 
	  Blue
	| Desert
	| Metal
	| Ocean
	deriving (Show, Eq)

ecotypeAssetName' :: Ecotype -> String
ecotypeAssetName' Blue   = "planet1"
ecotypeAssetName' Desert = "planet1"
ecotypeAssetName' Metal  = "planet1"
ecotypeAssetName' Ocean  = "planet1"
ecotypeAssetName :: Getter Ecotype String
ecotypeAssetName = to ecotypeAssetName'

data Resources = Resources 
	{	_resFuel       :: Int
	,	_resMetal      :: Int
	,	_resAntimatter :: Int
	}

makeLenses ''Resources
makeLenses ''Planet
makeLenses ''Sector
derive makeBinary ''Ecotype

res a b c = Resources {_resFuel = a, _resMetal = b, _resAntimatter = c}

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
	,	_sectorSize        = (200, 200)
	,	_sectorSpawnPoints = [(10,10), (10,190), (190,190), (190,10)]
	,	_sectorPlanets     = Map.fromList
		[	(1, Planet {_planetID = 1, _planetName = "Splearth" , _planetEcotype = Blue  , _planetLocation  = (10 , 100), _planetResources = res 10 10 0})
		,	(2, Planet {_planetID = 2, _planetName = "Tatooine" , _planetEcotype = Desert, _planetLocation  = (100, 100), _planetResources = res 10 10 0})
		,	(3, Planet {_planetID = 3, _planetName = "Qoruscant", _planetEcotype = Metal , _planetLocation  = (190, 190), _planetResources = res 10 0 10})
		]
	,	_sectorSpaceLanes  = [(1,2), (2,3)]
	}
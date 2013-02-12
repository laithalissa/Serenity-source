{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game where

import Serenity.Debug(trace')

import AssetsManager
import Serenity.Model.Entity
import Serenity.Model.Fleet
import Serenity.Model.Sector

import Control.Lens
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map

data Game = Game
	{	_gameTime :: Double
	,	_gameRandom :: StdGen
	,	_gameSector :: Sector
	,	_gameNextEntityId :: Int
	,	_gameShips  :: Map EntityID (Entity Ship)
	,	_gameShipClasses :: Map String ShipClass
	,	_gameWeapons :: Map String Weapon
	,	_gameSystems :: Map String System
	}
	deriving Show
makeLenses ''Game

addShip :: OwnerID -> Ship -> Game -> Game
addShip ownerId ship game = game'
	where
	game' = gameNextEntityId .~ eId' $ (gameShips .~ ships' $ game)
	eId' = eId+1
	eId = game^.gameNextEntityId
	ships' = Map.insert eId entity' (game^.gameShips)
	entity' = Entity
		{	_entityID=eId
		,	_ownerID=ownerId
		,	_entityData=ship
		}

initGame :: Map OwnerID Fleet -> Addons -> Sector -> Game
initGame fleets addons sector = game'
	where 
	fleetsList :: [(OwnerID, Fleet)]
	fleetsList = Map.toList fleets
	spawnPoints :: [(Double, Double)]
	spawnPoints = sector^.sectorSpawnPoints
	fleetsSpawnPoint :: [(OwnerID, Fleet, Double, Double)]
	fleetsSpawnPoint = zipWith (\(oId, f) (x,y) -> (oId, f, x, y)) fleetsList spawnPoints
	shipsSpawnPoint :: [(OwnerID, ShipConfiguration, Double, Double)]
	shipsSpawnPoint = concat $ map f fleetsSpawnPoint
		where
		f (oId,Fleet scs,x,y) = map (\sc-> (oId,sc,x,y)) scs
	game' :: Game
	game' = foldl f game shipsSpawnPoint
		where
		f g (oId,sc,x,y) = addShip oId (initShip sc (x,y) (0,0)) g
		
	game = Game
		{	_gameTime = 0
		,	_gameRandom = mkStdGen 1758836
		,	_gameSector = sector
		,	_gameNextEntityId=0
		,	_gameShips  = Map.empty
		,	_gameShipClasses = _addonsShipClasses addons
		,	_gameWeapons = _addonsWeapons addons
		,	_gameSystems = _addonsSystems addons
		}

demoGame :: Addons -> Game
demoGame addons = game' game
	where 
	game = initGame fleets addons sectorOne
	fleets = Map.fromList 
		[	(0, demoFleet)
		,	(1, demoFleet)
		,	(2, demoFleet)
		,	(3, demoFleet)
		]
	game' game = gameShips .~ (Map.map f (game^.gameShips)) $ game
		where
		f :: Entity Ship -> Entity Ship
		f e = entityData.shipOrder .~ (OrderAttack $ (getEntity ((e^.ownerID + 1) `mod` 4))^.entityID) $ e
		getEntity :: OwnerID -> Entity Ship
		getEntity oId = foldl1 (\x y -> if x^.ownerID == oId then x else y) $ Map.elems (game^.gameShips)
-- demoGame addons = (initGame addons sectorOne){_gameShips=Map.fromList entities}
-- 	where
-- 	entities =
-- 		[	createEntity 0 (25,25) OrderNone 0 "Vic"
-- 		,	createEntity 1 (25,75) OrderNone 1 "Jon"
-- 		,	createEntity 2 (75,75) (OrderAttack 1) 2 "Squidballs"
-- 		,	createEntity 3 (75,25) OrderNone 3 "Laith"
-- 		]

-- 	createEntity eid location order player name =
-- 		(eid, Entity
-- 		{	_entityID = eid
-- 		,	_ownerID = player
-- 		,	_entityData = createShip location order name
-- 		})

-- 	createShip location order name = Ship
-- 		{	_shipName = name
-- 		,	_shipConfiguration = configuration
-- 		,	_shipLocation = location
-- 		,	_shipDirection = (0,1)
-- 		,	_shipDamage = healthExample
-- 		,	_shipOrder = order
-- 		,	_shipGoal = GoalNone
-- 		,	_shipPlan = []
-- 		,	_shipBeamTargets = []
-- 		}
-- 		where
-- 		configuration = ShipConfiguration
-- 			{	_shipConfigurationShipClass="Destroyer"
-- 			,	_shipConfigurationWeapons=[]
-- 			,	_shipConfigurationSystems=[]
-- 			}
-- 		healthExample = Damage
-- 			{	_damageHull = 0
-- 			,	_damageShield = 20
-- 			}


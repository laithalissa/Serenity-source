{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game where

import AssetsManager
import Serenity.Model.Entity
import Serenity.Model.Sector

import Control.Lens
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map

data Game = Game
	{	_gameTime :: Double
	,	_gameRandom :: StdGen
	,	_gameSector :: Sector
	,	_gameShips  :: Map EntityID (Entity Ship)
	,	_gameShipClasses :: Map String ShipClass
	,	_gameWeapons :: Map String Weapon
	,	_gameSystems :: Map String System
	}
	deriving Show
makeLenses ''Game

initGame :: Addons -> Sector -> Game
initGame addons sector = Game
	{	_gameTime = 0
	,	_gameRandom = mkStdGen 1758836
	,	_gameSector = sector
	,	_gameShips  = Map.empty
	,	_gameShipClasses = addons^.addonsShipClasses
	,	_gameWeapons = addons^.addonsWeapons
	,	_gameSystems = addons^.addonsSystems
	}

defaultGame = Game
	{	_gameTime = 0
	,	_gameRandom = mkStdGen 1758836
	,	_gameSector = sectorOne
	,	_gameShips  = Map.empty
	,	_gameShipClasses = Map.empty
	,	_gameWeapons = Map.empty
	,	_gameSystems = Map.empty
	}

demoGame addons = (initGame addons sectorOne){_gameShips=Map.fromList entities}
	where
	entities =
		[	createEntity 0 (25,25) OrderNone 0 "Vic"
		,	createEntity 1 (25,75) OrderNone 1 "Jon"
		,	createEntity 2 (75,75) (OrderAttack 1) 2 "Squidballs"
		,	createEntity 3 (75,25) OrderNone 3 "Laith"
		]

	createEntity eid location order player name =
		(eid, Entity
		{	_entityID = eid
		,	_ownerID = player
		,	_entityData = createShip location order name
		})

	createShip location order name = Ship
		{	_shipName = name
		,	_shipConfiguration = configuration
		,	_shipLocation = location
		,	_shipDirection = (0,1)
		,	_shipDamage = healthExample
		,	_shipOrder = order
		,	_shipGoal = GoalNone
		,	_shipPlan = []
		,	_shipBeamTargets = []
		}
		where
		configuration = ShipConfiguration
			{	_shipConfigurationShipClass="Destroyer"
			,	_shipConfigurationWeapons=[]
			,	_shipConfigurationSystems=[]
			}
			healthExample = Damage
			{	_damageHull = 0
			,	_damageShield = 20
			}


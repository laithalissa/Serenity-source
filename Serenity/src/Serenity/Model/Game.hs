{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game where

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
	}
	deriving Show
makeLenses ''Game

defaultGame = Game
	{	_gameTime = 0
	,	_gameRandom = mkStdGen 1758836
	,	_gameSector = sectorOne
	,	_gameShips  = Map.fromList []
	}

demoGame = Game
	{	_gameTime = 0
	,	_gameSector = sectorOne
	,	_gameShips = Map.fromList entities
	}
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
			,	_shipType = shipTypeExample
			,	_shipLocation = location
			,	_shipDirection = (0,1)
			,	_shipDamage = healthExample
			,	_shipOrder = order
			,	_shipGoal = GoalNone
			,	_shipPlan = []
			,	_shipBeamTargets = []
			}

		shipTypeExample = ShipType
			{	_shipTypeFrontWeaponSlots = 1
			,	_shipTypeSideWeaponSlots = 2
			,	_shipTypeDorsalWeaponSlots = 0
			,	_shipTypeSystemUpgradeSlots = 0
			,	_shipTypeMaxDamage = healthMaxExample 
			}

		healthMaxExample = Damage
			{	_damageHull = 200
			,	_damageShield = 300
			}

		healthExample = Damage
			{	_damageHull = 50
			,	_damageShield = 20
			}

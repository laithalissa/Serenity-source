{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game
(	Game
,	gameSector
,	gameShips
,	demoGame
) where

import Serenity.Model.Entity
import Serenity.Model.Sector

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

data Game = Game
	{	_gameSector :: Sector
	,	_gameShips  :: Map EntityID (Entity Ship)
	}
	deriving Show
makeLenses ''Game

defaultGame = Game
	{	_gameSector = sectorOne
	,	_gameShips  = Map.fromList []
	}

demoGame = Game
	{	_gameSector = sectorOne
	,	_gameShips = Map.fromList entities
	}
	where
		entities =
			[	createEntity 0 (25,25) OrderNone 0 "Vic"
			,	createEntity 1 (25,75) (OrderMove (0,0) Nothing) 1 "Jon"
			,	createEntity 2 (75,75) OrderNone 2 "Squidballs"
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
			,	_shipLocation = location
			,	_shipDirection = (0,1)
			,	_shipDamage = Damage 0 0
			,	_shipOrder = order
			,	_shipGoal = GoalNone
			,	_shipPlan = []
			}
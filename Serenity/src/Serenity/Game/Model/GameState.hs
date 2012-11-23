module Serenity.Game.Model.GameState
(	GameState(..)
,	initialize
,	step
,	handleMessage
,	gameMap
) where

import Serenity.Game.Model.ClientMessage(WorldMessage)
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.Entity(Entity)
import Serenity.Game.Model.GameMap(GameMap)

data GameState =
	GameState
	{	gameMap :: GameMap
	,	entities :: [Entity]
	}
	deriving (Show, Eq)

initialize :: GameMap -> GameState
initialize gameMap =
	GameState
	{	gameMap=gameMap
	,	entities=[]
	}

step :: TimeDuration -> GameState -> GameState
step timeDelta world = world

handleMessage :: WorldMessage -> GameState -> GameState
handleMessage message world = world

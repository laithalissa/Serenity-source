module Serenity.Game.Server.World
(	World(..)
,	initialize
,	step
,	handleMessage
,	gameMap
) where

import Serenity.Game.Model.ClientMessage(WorldMessage)
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.Entity(Entity)
import Serenity.Game.Model.GameMap(GameMap)

initialize :: GameMap -> World
step :: TimeDuration -> World -> World
handleMessage :: WorldMessage -> World -> World

data World =
	World
	{	gameMap :: GameMap
	,	entities :: [Entity]
	}
	deriving (Show, Eq)

initialize gameMap = 
	World  
	{	gameMap=gameMap
	,	entities=[]
	}

step timeDelta world = world
handleMessage message world = world

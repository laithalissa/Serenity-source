
module Serenity.Game.Server.World
(	World
,	initialize
,	step
,	handleMessage
,	gameMap
) where

import Serenity.Game.Server.GameMap(GameMap)
import Serenity.Game.Server.ClientMessage(WorldMessage)
import Serenity.Game.Model.Entity(Entity)


initialize :: GameMap -> World
step :: TimeDuration -> World -> World
handleMessage :: WorldMessage -> World

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

module Serenity.Game.Shared.Model.GameState
(	GameState(..)
,	initialize
) where

import Serenity.Game.Shared.Model.Common(TimeDuration)
import Serenity.Game.Shared.Model.Entity(Entity)
import Serenity.Game.Shared.Model.GameMap(GameMap)

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

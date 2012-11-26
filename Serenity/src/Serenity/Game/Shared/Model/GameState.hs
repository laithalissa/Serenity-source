module Serenity.Game.Shared.Model.GameState
(	GameState(..)
,	GameMap(..)
,	initialize
,	addEntity
,	removeEntity
,	getEntityById
,	hasEntity
,	hasEntityId
,	exampleGameState
) where

import Serenity.Game.Shared.Model.Common(TimeDuration)
import Serenity.Game.Shared.Model.Entity(GameEntity(..), Entity(..))
import Serenity.Game.Shared.Model.GameMap(GameMap(..), exampleGameMap)
import Serenity.Game.Shared.Model.Common(EntityId)
import Serenity.Game.Shared.Model.ShipOrder(ShipOrder(..), ShipOrderState(..))

import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map(Map)

data GameState =
	GameState
	{	gameStateGameMap :: GameMap -- ^ GameMap
	,	gameStateEntities :: Set GameEntity -- ^ Entities in game
	}
	deriving (Show, Eq)



initialize :: GameMap -> GameState
initialize gameMap =
	GameState
	{	gameStateGameMap=gameMap
	,	gameStateEntities= Set.empty
	}


getEntityById :: EntityId -> GameState -> Maybe GameEntity
getEntityById eId gameState = 
	if Set.null results 
		then Nothing
		else (Just . List.head . Set.toList) results
	where
		results = Set.filter 
				(\x -> entityId x == eId) 
				(gameStateEntities gameState)
	

hasEntity :: GameEntity -> GameState -> Bool
hasEntity entity gameState = hasEntityId (entityId entity) gameState

hasEntityId :: EntityId -> GameState -> Bool
hasEntityId entityId gameState = case (getEntityById entityId gameState) of
	Nothing -> False
	Just _ -> True

addEntity :: GameEntity -> GameState -> GameState
addEntity entity gameState = gameState{gameStateEntities=(Set.insert entity (gameStateEntities gameState))}

removeEntity :: GameEntity -> GameState -> GameState
removeEntity entity gameState = gameState{gameStateEntities=(Set.delete entity (gameStateEntities gameState))}

isValid :: GameState -> Bool
isValid _ = True

exampleGameState = GameState
	{ gameStateGameMap = exampleGameMap
	, gameStateEntities = Set.fromList entities
	}

	where
		entities = 
			[	createGameEntity 0 (25,25) StayStillOrderState "Laith"
			,	createGameEntity 1 (25,75) StayStillOrderState "Jon"
			,	createGameEntity 2 (75,75) StayStillOrderState "Joseph"
			,	createGameEntity 3 (75,25) StayStillOrderState "Vic"
			]

		createGameEntity eid location order player = 
			GameEntity
			{	entityId = eid
			,	ownerId = player
			,	entity = createEntity location order
			}
			
		createEntity location order = 	
			Ship
			{	shipClass=1
			,	shipLocation=location
			,	shipDirection=(0,1)
			,	shipSpeed=(0,1)
			,	shipOrderState=order
			}

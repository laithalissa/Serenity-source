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

import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.List as List

data GameState =
	GameState
	{	gameStateGameMap :: GameMap
	,	gameStateEntities :: Set GameEntity
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

exampleGameState = foldl f (initialize exampleGameMap) entities
	where
		f = flip addEntity
		entities = 
			[	createGameEntity 0 (5,5) StayStillOrder
			,	createGameEntity 1 (10,5) StayStillOrder
			,	createGameEntity 2 (90,45) MoveOrder{moveOrderLocation=(10,10)}
			]

		createGameEntity eid location order = 
			GameEntity
			{	entityId=eid
			,	ownerId="ALC"
			,	entity=createEntity location order
			}
			
		createEntity location order = 	
			Ship
			{	shipClass=1
			,	shipLocation=location
			,	shipDirection=(0,1)
			,	shipSpeed=(0,1)
			,	shipOrder=order
			}

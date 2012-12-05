
module Serenity.Game.Server.EntityController
(	entityUpdateSelf
,	shipNewOrder
) where

import Serenity.Game.Server.Math(distance, unitVector)

import Serenity.Game.Shared.Model.GameState(GameState(..))
import Serenity.Game.Shared.Model.Entity
	(	GameEntity(..)
	,	Entity(..)
	,	setShipOrderState
	,	getShipOrderState
	)

import Serenity.Game.Shared.Model.ShipOrder
	(	ShipOrder(..)
	,	ShipOrderState(..)
	)

import Data.Graph.AStar(aStar)

import Serenity.Game.Shared.Model.Common(Location, Path)

import Serenity.Game.Shared.Model.GameMap
	(	GameMap(..)
	,	Planet(..)
	,	getPlanetNames
	,	getPlanetLocationByName
	,	getConnectedPlanets
	)

import Serenity.Game.Server.Math
	(	distance
	)

import Data.Maybe(fromJust)
import qualified Data.Set as Set

shipNewOrder :: GameState -> ShipOrder -> GameEntity -> GameEntity
shipNewOrder gameState StayStillOrder gameEntity = setShipOrderState StayStillOrderState gameEntity
shipNewOrder gameState (MoveOrder destination) gameEntity = 
	setShipOrderState (MoveOrderState path) gameEntity
		where
		gm = gameStateGameMap gameState
		start = shipLocation . entity 
		path :: Path
		--path = findPath gm (start gameEntity) destination
		path = [destination]
	


-- | finds the fastest route between two points in the GameMap
findPath :: GameMap -- ^ GameMap provides the graph to traverse
	-> Location -- ^ starting point
	-> Location -- ^ finish point
	-> Path -- ^ optimal path
findPath gameMap (sx, sy) (fx, fy) = (map loc $ fromJust $ aStar graph dist heur goal start) ++ [(fx, fy)]
	
	where
		graph planetName = (Set.fromList $ getConnectedPlanets planetName gameMap)
		dist planetName1 planetName2 = distance (loc planetName1) (loc planetName2)
		heur planetName = distance (loc planetName) (loc finish)
		goal = (==) (closestPlanet (fx,fy)) 
		loc n = getPlanetLocationByName n gameMap
		start = closestPlanet (sx, sy)
		finish = closestPlanet (fx, fy)

		closestPlanet :: Location -> String
		closestPlanet l = foldl1 pc $ getPlanetNames gameMap
			where
			pc p1 p2 = if (distance (loc p1) l) < (distance (loc p2) l) then p1 else p2
	





entityUpdateSelf :: GameState -> GameEntity -> GameEntity
entityUpdateSelf 
	gameState
	gEntity@GameEntity
	{	entityId=eId
	,	ownerId=oId
	,	entity=
			entity@Ship
			{	shipLocation=(x,y)
			,	shipSpeed=(speedX, speedY)
			,	shipOrderState=shipOrder
			}
	} = case shipOrder of
		StayStillOrderState -> gEntity{entity=entity{shipSpeed=(0,0)}}
		MoveOrderState path -> handleMove path

			
			
	where
	handleMove ((nx,ny):path) =  if (distance (x,y) (nx,ny)) < 3
			then if (null path)
				then shipNewOrder gameState StayStillOrder gEntity
				else 
					gEntity
					{	entity=entity
						{	shipOrderState=
							MoveOrderState
							{	moveOrderStatePath=path
							}
						,	shipSpeed=nSpeed
						,	shipDirection=nSpeed
						}
					}
			else gEntity{entity=entity{shipSpeed=nSpeed,shipDirection=nSpeed}}
		where
		nSpeed = 10 * (unitVector (nx-x, ny-y))





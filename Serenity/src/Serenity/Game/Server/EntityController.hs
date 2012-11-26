
module Serenity.Game.Server.EntityController
(	entityUpdateSelf
,	shipNewOrder
) where

import Serenity.Game.Server.Math(distance, unitVector)

import Serenity.Game.Shared.Model.GameState(GameState)
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



shipNewOrder :: GameState -> ShipOrder -> GameEntity -> GameEntity
shipNewOrder gameState StayStillOrder gameEntity = setShipOrderState StayStillOrderState gameEntity
shipNewOrder gameState (MoveOrder destination) gameEntity = 
	setShipOrderState (MoveOrderState [(250, 250), destination]) gameEntity
	


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
	handleMove ((nx,ny):path) =  if (distance (x,y) (nx,ny)) < 10 
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





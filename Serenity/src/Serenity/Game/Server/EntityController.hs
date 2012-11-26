
module Serenity.Game.Server.EntityController
(	makeDecision
) where

import Serenity.Game.Server.Math(distance, unitVector)

import Serenity.Game.Shared.Model.GameState(GameState)
import Serenity.Game.Shared.Model.Entity
	(	GameEntity(..)
	,	Entity(..)
	)

import Serenity.Game.Shared.Model.ShipOrder
	(	ShipOrder(..)
	)

makeDecision :: GameState -> GameEntity -> GameEntity
makeDecision 
	gameState
	gEntity@GameEntity
	{	entityId=eId
	,	ownerId=oId
	,	entity=
			entity@Ship
			{	shipLocation=(x,y)
			,	shipSpeed=(speedX, speedY)
			,	shipOrder=shipOrder
			}
	} = case shipOrder of
		StayStillOrder -> gEntity{entity=entity{shipSpeed=(0,0)}}
		MoveOrder{} -> handleMove shipOrder

			
			
	where
	handleMove
		order@MoveOrder
		{	moveOrderLocation=finalDestination
		,	moveOrderPath=Just ((nx,ny):path)
		} =  if (distance (x,y) (nx,ny)) < 10 
			then if (null path)
				then gEntity{entity=entity{shipOrder=StayStillOrder}}
				else 
					gEntity
					{	entity=entity
						{	shipOrder=
							MoveOrder
							{	moveOrderLocation=finalDestination
							,	moveOrderPath=Just path
							}
						,	shipSpeed=nSpeed
						,	shipDirection=nSpeed
						}
					}
			else gEntity{entity=entity{shipSpeed=nSpeed,shipDirection=nSpeed}}
		where
		nSpeed = 10 * (unitVector (nx-x, ny-y))





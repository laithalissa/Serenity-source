module Serenity.Game.Shared.Model.Entity
(	module Serenity.Game.Shared.Model.ShipOrder
,	GameEntity(..)
,	Entity(..)
,	setShipOrderState
,	getShipOrderState
) where

import Serenity.Game.Shared.Model.ShipOrder
import Serenity.Game.Shared.Model.ShipClass
import Serenity.Game.Shared.Model.Common(EntityId, Location, Direction, Speed, OwnerId)
import Data.Ord(Ordering(LT, EQ, GT))

data GameEntity = 
	GameEntity
	{	entityId :: EntityId
	,	ownerId :: OwnerId
	,	entity :: Entity
	} deriving(Show)

instance Eq GameEntity where
	a == b = (entityId a) == (entityId b)

instance Ord GameEntity where
	compare a b = compare (entityId a) (entityId b)

data Entity =
	Ship
	{	shipClass :: Int
	,	shipLocation :: Location
	,	shipDirection :: Direction
	,	shipSpeed :: Speed
	,	shipOrderState :: ShipOrderState
	}
	| Gun
	{	shipId :: EntityId
	,	weaponSlotIndex :: Int
	}
	| System
	{	shipId :: EntityId
	}
	| Bullet
	{	bulletLocation :: Location
	,	bulletDirection :: Direction
	,	bulletSpeed :: Float
	}
	deriving (Show)



setShipOrderState :: ShipOrderState -> GameEntity -> GameEntity
setShipOrderState orderState ge@GameEntity{entity=entity} = ge{entity=entity{shipOrderState=orderState}}

getShipOrderState :: GameEntity -> ShipOrderState
getShipOrderState ge@GameEntity{entity=entity@Ship{shipOrderState=orderState}} = orderState


module Serenity.Game.Shared.Model.Entity
(	module Serenity.Game.Shared.Model.ShipOrder
,	GameEntity(..)
,	Entity(..)
) where

import Serenity.Game.Shared.Model.ShipOrder
import Serenity.Game.Shared.Model.ShipClass
import Serenity.Game.Shared.Model.Common
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
	,	shipSpeed :: Direction
	,	shipOrder :: ShipOrder
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




module Serenity.Game.Model.Entity
(	Entity(..)
,	module Serenity.Game.Model.ShipOrder
) where

import Serenity.Game.Model.ShipOrder
import Serenity.Game.Model.ShipClass
import Serenity.Game.Model.Common

data Entity =
	Ship
	{	shipId :: Int
	,	shipClass :: Int
	,	shipLocation :: Location
	,	shipDirection :: Direction
	,	shipSpeed :: Direction
	,	shipOrder :: ShipOrder
	}
	| Gun
	{	shipId :: Int
	,	weaponSlotIndex :: Int
	}
	| System
	{	shipId :: Int
	}
	| Bullet
	{	bulletLocation :: Location
	,	bulletDirection :: Direction
	,	bulletSpeed :: Float
	}
	deriving (Show, Eq)


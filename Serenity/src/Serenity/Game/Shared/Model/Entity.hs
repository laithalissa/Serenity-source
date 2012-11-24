module Serenity.Game.Shared.Model.Entity
(	Entity(..)
,	module Serenity.Game.Shared.Model.ShipOrder
) where

import Serenity.Game.Shared.Model.ShipOrder
import Serenity.Game.Shared.Model.ShipClass
import Serenity.Game.Shared.Model.Common

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


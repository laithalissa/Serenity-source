module Serenity.Game.Model.Entity where

import Serenity.Game.Model.ShipOrder
import Serenity.Game.Model.Common

data Entity =
	Ship
	{	shipId :: Int
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
	{	location :: Location
	,	direction :: Direction
	,	speed :: Float
	}
	deriving (Show, Ord, Eq)

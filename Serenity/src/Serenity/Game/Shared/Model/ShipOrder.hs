module Serenity.Game.Shared.Model.ShipOrder where

import Serenity.Game.Shared.Model.Common

data ShipOrder =
	  StayStillOrder 
	| MoveOrder 
	{	moveOrderLocation :: Location 
	}
	deriving(Show, Eq, Ord)


data ShipOrderState = 
	StayStillOrderState
	| MoveOrderState
	{	moveOrderStatePath :: Path
	} deriving(Show, Eq, Ord)
	

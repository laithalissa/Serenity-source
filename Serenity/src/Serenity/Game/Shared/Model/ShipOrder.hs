module Serenity.Game.Shared.Model.ShipOrder where

import Serenity.Game.Shared.Model.Common

data ShipOrder =
	  StayStillOrder 
	| MoveOrder 
	{	moveOrderLocation :: Location 
	,	moveOrderPath :: Maybe Path
	}
	deriving(Show, Eq, Ord)

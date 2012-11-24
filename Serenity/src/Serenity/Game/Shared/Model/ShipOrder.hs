module Serenity.Game.Shared.Model.ShipOrder where

import Serenity.Game.Shared.Model.Common

data ShipOrder =
	  StayStillOrder 
	| MoveOrder { moveOrderLocation :: Location }
	deriving(Show, Eq, Ord)

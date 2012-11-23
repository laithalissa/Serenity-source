module Serenity.Game.Model.ShipOrder where

import Serenity.Game.Model.Common

data ShipOrder =
	  StayStillOrder 
	| MoveOrder { moveOrderLocation :: Location }
	deriving(Show, Eq, Ord)

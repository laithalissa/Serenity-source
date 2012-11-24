module Serenity.Game.Shared.Model.WorldDelta where

import Serenity.Game.Shared.Model.Entity

data WorldDelta =
	  EntityAdded String
	| EntityOrientation Entity
	deriving(Show)

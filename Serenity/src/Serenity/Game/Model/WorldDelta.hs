module Serenity.Game.Model.WorldDelta where

import Serenity.Game.Model.Entity

data WorldDelta =
	  EntityAdded String
	| EntityOrientation Entity
	deriving(Show)

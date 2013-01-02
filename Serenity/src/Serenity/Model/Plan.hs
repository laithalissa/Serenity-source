module Serenity.Model.Plan where

import Serenity.Model.Game
import Serenity.Model.Entity
import Serenity.Model.Order

import Data.Vinyl

plan :: Game -> Entity -> Plan
plan _ entity = case entity ^. (rLens order) of
	OrdereNone    -> PlanNone
	(OrderMove d) -> PlanMove d
	_             -> PlanNone

module Serenity.Model.Plan where

import Serenity.Model.Entity
import Serenity.Model.Game	
import Control.Lens

plan :: Game -> Entity Ship -> Plan
plan _ entity = case entity^.entityData.shipOrder of
	OrderNone     -> PlanNone
	(OrderMove d) -> PlanMove d
	_             -> PlanNone

{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Prelude hiding (id, (.))

import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Wire
import Serenity.Maths.Util

import Control.Lens

import Debug.Trace(trace)

goal :: Game -> Order -> Goal
goal _ (OrderNone)            = GoalNone
goal _ (OrderMove a b)        = GoalBeAt a b
goal _ (OrderAttack a)        = GoalDestroyed a
goal _ (OrderGuardShip a)     = GoalGuardShip a
goal _ (OrderGuardPlanet a)   = GoalGuardPlanet a 
goal _ (OrderGuardLocation a) = GoalGuardLocation a
goal _ (OrderCapture a)       = GoalCaptured a

plan :: Game -> Goal -> Plan
plan _ GoalNone = []
plan _ (GoalBeAt a (Just b)) = [ActionMove a b]
plan _ (GoalBeAt a Nothing)  = [ActionMove a (0,0)]
plan _ _ = []

actt :: Game -> UpdateWire (Entity Ship, ShipAction)
actt game = proc (entity, action) -> do 
	case action of
		ActionMove a b -> id -< [UpdateEntityLocation (entity^.entityID) (pDouble2Float $ currentLoc + 0.1(a-currentLoc))] 
			where currentLoc = entity^.entityData.shipLocation
		_ -> id -< []

evolveShip :: Game -> UpdateWire (Entity Ship)
evolveShip game = proc entity@Entity{_entityData=ship} -> do
	case trace (show $ ship^.shipPlan) ship^.shipPlan of
		action:_ -> actt game -< (entity, action)
		[] -> id -< [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] where
			g = goal game (ship^.shipOrder)
			p = plan game g
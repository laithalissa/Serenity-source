{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Prelude hiding (id, (.))

import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Wire
import Serenity.Maths.Util

import Control.Lens

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

actt :: UpdateWire (Entity Ship, ShipAction, Game)
actt = proc (entity, action, game) -> do 
	case action of
		ActionMove a b -> id -< [UpdateEntityLocation (entity^.entityID) (pDouble2Float $ currentLoc + 0.1(a-currentLoc))] 
			where currentLoc = entity^.entityData.shipLocation
		_ -> id -< []

evolveShip :: UpdateWire (Entity Ship, Game)
evolveShip = proc (entity@Entity{_entityData=ship}, game) -> do
	case ship^.shipPlan of
		action:_ -> actt -< (entity, action, game)
		[] -> id -< if p == [] then [] else [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] where
			g = goal game (ship^.shipOrder)
			p = plan game g
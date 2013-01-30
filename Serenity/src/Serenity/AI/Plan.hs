{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Prelude hiding (id, (.))

import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Wire
import Serenity.Maths.Util
import Serenity.AI.Path

import Control.Lens
import Data.VectorSpace

goal :: Game -> Order -> Goal
goal _ (OrderNone)            = GoalNone
goal _ (OrderMove a b)        = GoalBeAt a b
goal _ (OrderAttack a)        = GoalDestroyed a
goal _ (OrderGuardShip a)     = GoalGuardShip a
goal _ (OrderGuardPlanet a)   = GoalGuardPlanet a 
goal _ (OrderGuardLocation a) = GoalGuardLocation a
goal _ (OrderCapture a)       = GoalCaptured a

plan :: Game -> Entity Ship -> Goal -> Plan
plan _ _ GoalNone = []
plan game entity (GoalBeAt a (Just b)) = [ActionMove (game^.gameTime) (entity^.entityData.shipLocation, entity^.entityData.shipDirection) a b]
plan game entity (GoalBeAt a Nothing)  = [ActionMove (game^.gameTime) (entity^.entityData.shipLocation, entity^.entityData.shipDirection) a (0, 1)]
plan _ _ _ = []

evolveShip :: UpdateWire (Entity Ship, Game)
evolveShip = proc (entity@Entity{_entityData=ship}, game) -> do
	case ship^.shipPlan of
		[] -> id -< if p == [] then [] else [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] where
			g = goal game (ship^.shipOrder)
			p = plan game entity g
		(action:rest) -> if finished game ship action
			then id -< [UpdateShipPlan (entity^.entityID) rest]
			else actt -< (entity, action, game)

finished :: Game -> Ship -> ShipAction -> Bool
finished _ ship (ActionMove _ _ dest dir) = ((ship^.shipLocation) =~= dest) && ((ship^.shipDirection) =~= dir)
finished _ ship (ActionAttack a)    = True
finished _ ship (ActionCapture a)   = True

(=~=) :: (Double, Double) -> (Double, Double) -> Bool
x =~= y = magnitude (x-y) < 5

actt :: UpdateWire (Entity Ship, ShipAction, Game)
actt = proc (entity, action, game) -> do 
	case action of
		ActionMove t start dest dir -> move -< (entity, t, start, dest, dir)
		_ -> id -< []

move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), (Double, Double), (Double, Double))
move = proc (entity, startTime, start, dest, dir) -> do
	timeNow <- time -< ()
	id -< [UpdateEntityLocation (entity^.entityID) (pDouble2Float $ makePath 10 start (dest,dir) $ (timeNow-startTime)/100)]

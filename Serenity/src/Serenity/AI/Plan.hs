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
plan game entity (GoalBeAt a b) = [ActionMove (game^.gameTime) (entity^.entityData.shipLocation, entity^.entityData.shipDirection) a b'] where
	b' = case b of {Just x -> x; Nothing -> (0,1)}
plan _ _ _ = []

evolveShip :: UpdateWire (Entity Ship, Game)
evolveShip = proc (entity@Entity{_entityData=ship}, game) -> do
	case ship^.shipPlan of
		[] -> id -< 
			if finishedOrder game ship (ship^.shipOrder)
				then [UpdateShipOrder (entity^.entityID) OrderNone]
				else if p == [] then [] else [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] where
			g = goal game (ship^.shipOrder)
			p = plan game entity g
		(action:rest) -> if finishedAction game ship action
			then id -< [UpdateShipPlan (entity^.entityID) rest]
			else actt -< (entity, action, game)

finishedAction :: Game -> Ship -> ShipAction -> Bool
finishedAction _ ship (ActionMove _ _ dest dir) = ((ship^.shipLocation) =~= dest) -- && ((ship^.shipDirection) =~= dir)
finishedAction _ ship (ActionAttack a)  = True
finishedAction _ ship (ActionCapture a) = True

finishedOrder :: Game -> Ship -> Order -> Bool
finishedOrder _ _ OrderNone = False
finishedOrder _ ship (OrderMove dest mDir) = ((ship^.shipLocation) =~= dest) && x where
	x = case mDir of
		Just dir -> ((ship^.shipDirection) =~= dir)
		Nothing -> True
finishedOrder _ ship (OrderAttack a)        = True
finishedOrder _ ship (OrderGuardShip a)     = True
finishedOrder _ ship (OrderGuardPlanet a)   = True
finishedOrder _ ship (OrderGuardLocation a) = True
finishedOrder _ ship (OrderCapture a)       = True

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
	let curve = (makePath 15) start
	let position = curve (dest, dir) $ (timeNow-startTime)/10
	let position' = differentiate (curve (dest, dir), (timeNow-startTime)/10)
	id -< return UpdateEntityLocationDirection
		{	updateEntityID = entity^.entityID
		,	updateEntityLocation = pDouble2Float position
		,	updateEntityDirection = pDouble2Float position'
		}
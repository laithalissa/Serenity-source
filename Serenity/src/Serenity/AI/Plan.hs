{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Wire
import Serenity.Maths.Util
import Serenity.AI.Path

import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.VectorSpace
import Prelude hiding (id, (.))

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
plan game entity (GoalBeAt goalLoc mGoalDir) = [ActionMove (game^.gameTime) (shipLoc,shipDir) (goalLoc,goalDir)] where
	goalDir = case mGoalDir of {Just x -> x; Nothing -> normalized (goalLoc - shipLoc)}
	shipLoc = entity^.entityData.shipLocation
	shipDir = entity^.entityData.shipDirection
plan game entity (GoalDestroyed target) = [ActionMoveToEntity target (ActionMove (game^.gameTime) (shipLoc,shipDir) (goalLoc,goalDir))] where
	goalLoc = case game^.gameShips.at target of {Just e -> e^.entityData.shipLocation; Nothing -> shipLoc}
	goalDir = normalized (goalLoc - shipLoc)
	shipLoc = entity^.entityData.shipLocation
	shipDir = entity^.entityData.shipDirection
plan _ _ _ = []

evolveShipPlan :: UpdateWire (Entity Ship, Game)
evolveShipPlan = proc (entity@Entity{_entityData=ship}, game) -> do
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
finishedAction _ ship ActionMove{endLocDir=(dest,dir)} = ((ship^.shipLocation) =~= dest) -- && ((ship^.shipDirection) =~= dir)
finishedAction game ship (ActionAttack target) = True -- not ((game^.gameShips.contains target) && (inRange ship (fromJust $ game^.gameShips.at target)))
finishedAction _ ship (ActionCapture a) = True
finishedAction game ship (ActionMoveToEntity target _) = (not (game^.gameShips.contains target)) || ((ship^.shipLocation) =~= (game^.gameShips.(at target).(to fromJust).entityData.shipLocation))

finishedOrder :: Game -> Ship -> Order -> Bool
finishedOrder _ _ OrderNone = False
finishedOrder _ ship (OrderMove dest mDir) = ((ship^.shipLocation) =~= dest) && x where
	x = case mDir of
		Just dir -> ((ship^.shipDirection) =~= dir)
		Nothing -> True
finishedOrder game _ (OrderAttack target) = M.notMember target (game^.gameShips)
finishedOrder _ ship (OrderGuardShip a)     = True
finishedOrder _ ship (OrderGuardPlanet a)   = True
finishedOrder _ ship (OrderGuardLocation a) = True
finishedOrder _ ship (OrderCapture a)       = True

(=~=) :: (Double, Double) -> (Double, Double) -> Bool
x =~= y = magnitude (x-y) < 5

actt :: UpdateWire (Entity Ship, ShipAction, Game)
actt = proc (entity, action, game) -> do 
	case action of
		ActionMove {startTime = t, startLocDir=start, endLocDir=end} -> move -< (entity, t, start, end)
		(ActionMoveToEntity tID m) -> if isJust target
			then moveToEntity -< (entity, fromJust target, m, game)
			else id -< []
			where
				target = game^.gameShips.at tID
		_ -> id -< []

move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), ((Double, Double), (Double, Double)))
move = proc (entity, startTime, start, end) -> do
	timeNow <- time -< ()
	let (curve, curveLength) = makePath 15 start end
	let s = (timeNow-startTime)/(curveLength*0.1)
	let position = curve s
	let position' = differentiate (curve, s)
	id -< return UpdateEntityLocationDirection
		{	updateEntityID = entity^.entityID
		,	updateEntityLocation = pDouble2Float position
		,	updateEntityDirection = pDouble2Float position'
		}

-- | UpdateWire to move a ship towards another, possibly moving, entity.
--
-- If the currently planned end location is too far away from the current
-- position of the target then the move is re-planned.
moveToEntity :: UpdateWire (Entity Ship, Entity Ship, ShipAction, Game)
moveToEntity = proc (entity, target, action, game) -> do
	if magnitude ((fst $ endLocDir action) - (target^.entityData.shipLocation)) > 50
		|| finishedAction game (entity^.entityData) action
		then id -< [UpdateShipPlan (entity^.entityID) []]
		else move -< (entity, startTime action, startLocDir action, endLocDir action)

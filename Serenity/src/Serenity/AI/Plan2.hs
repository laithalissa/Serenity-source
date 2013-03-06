{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Serenity.Model.Common
import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Sector
import Serenity.Model.Wire
import Serenity.Maths.Util
import Serenity.AI.Path


import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.VectorSpace
import Prelude hiding (id, (.))

import Debug.Trace(trace)

evolveShipPlan :: UpdateWire (Entity Ship, Game) 
evolveShipPlan = switch chooseOrderWire orderNonePlan



destination :: Position
destination = ((150, 100), (1, 0))

---------- Orders ----------

-- the order*Plan functions will decide the best action to take given that order

chooseOrderWire :: BaseWire (Entity Ship, Game) (UpdateWire (Entity Ship, Game))
chooseOrderWire = proc (entity, game) -> case (entity^.entityData.shipOrder) of
		OrderNone{} -> id -< orderNonePlan
		OrderMove{} -> id -< orderMovePlan
		OrderAttack{} -> id -< orderAttackPlan

orderNonePlan :: UpdateWire (Entity Ship, Game)
orderNonePlan = proc (entity, game) -> do
	id -< []

orderMovePlan :: UpdateWire (Entity Ship, Game) 
orderMovePlan = proc (entity, game) -> do
	id -< []	

orderAttackPlan :: UpdateWire (Entity Ship, Game)
orderAttackPlan = proc (entity, game) -> do
	id -< []


---------- Goals ----------

-- the goal*Plan dictates what actions to perofrm given that goal

goalNonePlan :: UpdateWire (


---------- Actions ----------

-- the action*Plan generates updates given an action

actionMovePlan :: UpdateWire (Entity Ship, Game)
actionMovePlan = proc (entity, game) -> do
	


---------- Utils ----------




-- goal :: Game -> Order -> Goal
-- goal _ (OrderNone{})           					= GoalNone
-- goal _ (OrderMove{orderLocation=loc, orderDirection=dir}) 	= GoalBeAt loc dir
-- goal _ (OrderAttack{orderTargetEntityID=eID})        		= GoalDestroyed eID
-- goal _ (OrderGuardShip a)     = GoalGuardShip a
-- goal _ (OrderGuardPlanet a)   = GoalGuardPlanet a 
-- goal _ (OrderGuardLocation a) = GoalGuardLocation a
-- goal _ (OrderCapture a)       = GoalCaptured a

-- plan :: BaseWire (Game, Entity Ship, Goal) Plan
-- plan = proc (game, entity, goal) -> case goal of
-- 	GoalNone -> id -< []
-- 	(GoalBeAt goalLoc mGoalDir) -> id -< trace "goal planning" $ [actionMove1, actionMove2]
-- 		where
-- 		actionMove1 = ActionMove (game^.gameTime) (shipLoc, shipDir) ((100.0, 100.0), shipDir)
-- 		actionMove2 = ActionMove (game^.gameTime) ((100.0, 100.0), shipDir) (goalLoc,goalDir)
-- 		goalDir = case mGoalDir of {Just x -> x; Nothing -> normalized (goalLoc - shipLoc)}
-- 		shipLoc = entity^.entityData.shipLocation
-- 		shipDir = entity^.entityData.shipDirection
-- 	(GoalDestroyed target) -> id -< trace "goal destroyed" $ [ActionMoveToEntity target (ActionMove (game^.gameTime) (shipLoc,shipDir) (goalLoc,goalDir))] 
-- 		where
-- 		goalLoc = case game^.gameShips.at target of {Just e -> e^.entityData.shipLocation; Nothing -> shipLoc}
-- 		goalDir = normalized (goalLoc - shipLoc)
-- 		shipLoc = entity^.entityData.shipLocation
-- 		shipDir = entity^.entityData.shipDirection
-- 	_ -> id -< []




-- makeWaypoints :: BaseWire (Sector, Position, Position, Speed) [Location]
-- makeWaypoints = proc (sector, start, finish, speed) -> do
-- 	id -< [fst start, fst finish]

-- evolveShipPlan :: UpdateWire (Entity Ship, Game)
-- evolveShipPlan = proc (entity@Entity{_entityData=ship}, game) -> do
-- 	case ship^.shipPlan of
-- 		[] -> do
-- 			g <- id -< goal game (ship^.shipOrder)
-- 			p <- plan -< (game, entity, g)
-- 			id -< if finishedOrder game ship (ship^.shipOrder)
-- 				then [UpdateShipOrder (entity^.entityID) makeOrderNone]
-- 				else if p == [] then [] else [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] 

-- 		(action:rest) -> if finishedAction game ship action
-- 			then id -< [UpdateShipPlan (entity^.entityID) rest]
-- 			else actt -< (entity, action, game)

-- finishedAction :: Game -> Ship -> ShipAction -> Bool
-- finishedAction _ ship ActionMove{endLocDir=(dest,dir)} = ((ship^.shipLocation) =~= dest) -- && ((ship^.shipDirection) =~= dir)
-- finishedAction game ship (ActionAttack target) = True -- not ((game^.gameShips.contains target) && (inRange ship (fromJust $ game^.gameShips.at target)))
-- finishedAction _ ship (ActionCapture a) = True
-- finishedAction game ship (ActionMoveToEntity target _) = (not (game^.gameShips.contains target)) || ((ship^.shipLocation) =~= (game^.gameShips.(at target).(to fromJust).entityData.shipLocation))

-- finishedOrder :: Game -> Ship -> Order -> Bool
-- finishedOrder _ _ OrderNone{} = False
-- finishedOrder _ ship (OrderMove{orderLocation=dest, orderDirection=mDir}) = ((ship^.shipLocation) =~= dest) && x 
-- 	where
-- 	x = case mDir of
-- 		Just dir -> ((ship^.shipDirection) =~= dir)
-- 		Nothing -> True
-- finishedOrder game _ (OrderAttack target) = M.notMember target (game^.gameShips)
-- finishedOrder _ ship (OrderGuardShip a)     = True
-- finishedOrder _ ship (OrderGuardPlanet a)   = True
-- finishedOrder _ ship (OrderGuardLocation a) = True
-- finishedOrder _ ship (OrderCapture a)       = True

-- (=~=) :: (Double, Double) -> (Double, Double) -> Bool
-- x =~= y = magnitude (x-y) < 5

-- actt :: UpdateWire (Entity Ship, ShipAction, Game)
-- actt = proc (entity, action, game) -> do
-- 	case action of
-- 		ActionMove {startTime = t, startLocDir=start, endLocDir=end} -> move -< (entity, t, start, end, entitySpeed entity game)
-- 		(ActionMoveToEntity tID m) -> if isJust target
-- 			then moveToEntity -< (entity, fromJust target, m, game)
-- 			else id -< []
-- 			where
-- 				target = game^.gameShips.at tID
-- 		_ -> id -< []

-- entitySpeed :: Entity Ship -> Game -> Double
-- entitySpeed ship game = (shipClass' ship game)^.shipClassSpeed

-- -- move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), ((Double, Double), (Double, Double)), Double)
-- -- move = proc (entity, startTime, start, end, speed) -> do
-- -- 	moveSubGoal -< (entity, startTime, start, end, speed)

-- radiusOfCurvature = 15

-- moveTimeRemaining :: BaseWire (Double, Position, Position, Speed) Double
-- moveTimeRemaining = proc (startTime, start, end, speed) -> do
-- 	timeNow <- time -< ()
-- 	let (curve, curveLength) = makePath radiusOfCurvature start end
-- 	timeConsumed <- id -< (timeNow - startTime)
-- 	totalTime <- id -< (curveLength / speed)
-- 	timeRemaining <- id -< (totalTime - timeConsumed)
-- 	id -< timeRemaining

-- move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), ((Double, Double), (Double, Double)), Double)
-- -- move = proc (entity, startTime, start, end, speed) -> do
-- -- 	timeNow <- time -< ()
-- -- 	let (curve, curveLength) = makePath radiusOfCurvature start end
-- -- 	let s = ((timeNow-startTime)/(curveLength)) * speed -- * speed
-- -- 	let position = curve s
-- -- 	let position' = differentiate (curve, s)
-- -- 	id -< return UpdateEntityLocationDirection
-- -- 		{	updateEntityID = entity^.entityID
-- -- 		,	updateEntityLocation = pDouble2Float position
-- -- 		,	updateEntityDirection = pDouble2Float position'
-- -- 		}

-- move startTime = proc (entity, start, end, speed) -> do
-- 	timeNow <- time -< ()
-- 	let (curve, curveLength) = makePath radiusOfCurvature start end
-- 	let s = ((timeNow-startTime)/(curveLength)) * speed -- * speed
-- 	let position = curve s
-- 	let position' = differentiate (curve, s)
-- 	id -< return UpdateEntityLocationDirection
-- 		{	updateEntityID = entity^.entityID
-- 		,	updateEntityLocation = pDouble2Float position
-- 		,	updateEntityDirection = pDouble2Float position'
-- 		}

-- -- | UpdateWire to move a ship towards another, possibly moving, entity.
-- --
-- -- If the currently planned end location is too far away from the current
-- -- position of the target then the move is re-planned.
-- moveToEntity :: UpdateWire (Entity Ship, Entity Ship, ShipAction, Game)
-- moveToEntity = proc (entity, target, action, game) -> do
-- 	if magnitude ((fst $ endLocDir action) - (target^.entityData.shipLocation)) > 50
-- 		|| finishedAction game (entity^.entityData) action
-- 		then id -< [UpdateShipPlan (entity^.entityID) []]
-- 		else move -< (entity, startTime action, startLocDir action, endLocDir action, entitySpeed entity game)
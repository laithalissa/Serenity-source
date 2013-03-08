{-# LANGUAGE Arrows #-}

module Serenity.AI.Plan where

import Serenity.Model.Common
import Serenity.Model.Entity
import Serenity.Model.Game
import Serenity.Model.Message 
import Serenity.Model.Sector
import Serenity.Model.Wire
import Serenity.Maths.Util
import Serenity.AI.Navigation
import Serenity.AI.Path


import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.VectorSpace
import Prelude hiding (id, (.))

import Debug.Trace(trace)

arr' = arr . uncurry

goal :: Game -> Order -> Goal
goal _ (OrderNone{})           					= GoalNone
goal _ (OrderMove{orderLocation=loc, orderDirection=dir}) 	= GoalBeAt loc dir
goal _ (OrderAttack{orderTargetEntityID=eID})        		= GoalDestroyed eID
goal _ (OrderGuardShip a)     = GoalGuardShip a
goal _ (OrderGuardPlanet a)   = GoalGuardPlanet a 
goal _ (OrderGuardLocation a) = GoalGuardLocation a
goal _ (OrderCapture a)       = GoalCaptured a

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = (\(a,b,c) -> f a b c)

plan :: BaseWire (Game, Entity Ship, Goal) Plan
plan = proc (game, entity, goal) -> case goal of
	GoalNone -> id -< []
	(GoalBeAt goalLoc mGoalDir) -> do
		let sector = game^.gameMap'
		let startPosition = (entity^.entityData.shipLocation, entity^.entityData.shipDirection)
		let finishPosition = (goalLoc, if isJust mGoalDir then fromJust mGoalDir else makeDirection entity goalLoc)
		let shipSpeed = (shipClass' entity game)^.shipClassSpeed
		makeWaypoints -< (sector, startPosition, finishPosition, shipSpeed) 

	(GoalDestroyed target) -> id -< trace "goal destroyed" $ [ActionMoveToEntity target (ActionMove (game^.gameTime) (shipLoc,shipDir) (goalLoc,goalDir) False)]
		where
		goalLoc = case game^.gameShips.at target of {Just e -> e^.entityData.shipLocation; Nothing -> shipLoc}
		goalDir = normalized (goalLoc - shipLoc)
		shipLoc = entity^.entityData.shipLocation
		shipDir = entity^.entityData.shipDirection
	_ -> id -< []


makeDirection :: Entity Ship -> Location -> Direction
makeDirection entity goalLoc = let 
	shipLoc = entity^.entityData.shipLocation
	shipDir = entity^.entityData.shipDirection
	in normalized (goalLoc - shipLoc)

makeWaypoints :: BaseWire (Sector, Position, Position, Speed) [ShipAction]
makeWaypoints = proc (sector, start@(startPos, startDir), finish@(finishPos, finishDir), speed) -> do
	now <- time -< ()
	(planetID, distance) <- nearestPlanet -< (sector, startPos)
	planetLocation <- arr' planetLocation' -< (sector, planetID)
	id -< [ActionMove now start (planetLocation, finishDir) False]
		-- let shipSpeed = (shipClass' entity game)^.shipClassSpeed
		-- let shipStartLocation = entity^.entityData.shipLocation
		-- let shipStartDirection = entity^.entityData.shipDirection
		-- let shipStartPosition = (shipStartLocation, shipStartDirection)
		-- let shipStartTime = game^.gameTime
		-- let shipMidLocation = (100.0, 100.0)
		-- let shipMidDirection = makeDirection entity shipMidLocation
		-- let shipMidPosition = (shipMidLocation, shipMidDirection)
		-- timeRemaining <- moveTimeRemaining -< (game^.gameTime, shipStartPosition, shipMidPosition, shipSpeed) 
		-- let shipMidTime = shipStartTime + timeRemaining
		-- let shipFinishLocation = goalLoc
		-- let shipFinishDirection = if isJust mGoalDir then fromJust mGoalDir else makeDirection entity goalLoc
		-- let shipFinishPosition = (shipFinishLocation, shipFinishDirection)
		-- let action1 = ActionMove shipStartTime shipStartPosition shipMidPosition
		-- let action2 = ActionMove shipMidTime shipMidPosition shipFinishPosition
		-- let actionPlan = [action1, action2]
		-- id -< actionPlan



evolveShipPlan :: UpdateWire (Entity Ship, Game)
evolveShipPlan = proc (entity@Entity{_entityData=ship}, game) -> do
	case ship^.shipPlan of
		[] -> do
			g <- id -< goal game (ship^.shipOrder)
			p <- plan -< (game, entity, g)
			id -< if finishedOrder game ship (ship^.shipOrder)
				then [UpdateShipOrder (entity^.entityID) makeOrderNone]
				else if p == [] then [] else [UpdateShipGoal (entity^.entityID) g, UpdateShipPlan (entity^.entityID) p] 

		(action:rest) -> do
			hasFinishedAction <- finishedAction -< (game, entity, action)
			if hasFinishedAction
				then id -< [UpdateShipPlan (entity^.entityID) rest]
				else actt -< (entity, action, game)

--moveTimeRemaining :: BaseWire (Game, Entity Ship, Position, Double) Double
finishedAction :: BaseWire (Game, Entity Ship, ShipAction) Bool
finishedAction = proc (game, entity, action) -> case action of
	-- ActionMofve{actionFinishPosition=(location,direction)} -> do
		
	-- 	id -< ((ship^.shipLocation) =~= location)
	ActionMove{actionFinishPosition=finish, actionStartTime=time} -> do
		remainingTime <- moveTimeRemaining -< (game, entity, finish, time)
		id -< (remainingTime < 0.5)
	ActionAttack{actionTargetID=target} -> id -< True
	ActionCapture{} -> id -< True	
	(ActionMoveToEntity target _) -> do
		hasTarget <- id -< (not (game^.gameShips.contains target))
		atTarget <- id -< ((entity^.entityData.shipLocation) =~= (game^.gameShips.(at target).(to fromJust).entityData.shipLocation))
		id -< hasTarget || atTarget



finishedOrder :: Game -> Ship -> Order -> Bool
finishedOrder _ _ OrderNone{} = False
finishedOrder _ ship (OrderMove{orderLocation=dest, orderDirection=mDir}) = ((ship^.shipLocation) =~= dest) && x 
	where
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
		ActionMove {actionStartTime=t, actionStartPosition=start, actionFinishPosition=end} -> move -< (entity, t, start, end, shipSpeed' game entity )
		(ActionMoveToEntity tID m) -> if isJust target
			then moveToEntity -< (entity, fromJust target, m, game)
			else id -< []
			where
				target = game^.gameShips.at tID
		_ -> id -< []


-- move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), ((Double, Double), (Double, Double)), Double)
-- move = proc (entity, startTime, start, end, speed) -> do
-- 	moveSubGoal -< (entity, startTime, start, end, speed)

radiusOfCurvature = 15

moveTimeRemaining :: BaseWire (Game, Entity Ship, Position, Double) Double
moveTimeRemaining = proc (game, entity, end, startTime)  -> do
	speed <- arr' shipSpeed' -< (game, entity)
	start <- arr (\e-> (e^.entityData.shipLocation, e^.entityData.shipDirection)) -< entity
	timeNow <- time -< ()
	let (curve, curveLength) = makePath radiusOfCurvature start end
	timeConsumed <- id -< (timeNow - startTime)
	totalTime <- id -< (curveLength / speed)
	timeRemaining <- id -< (totalTime - timeConsumed)
	id -< timeRemaining

move :: UpdateWire (Entity Ship, Double, ((Double,Double),(Double,Double)), ((Double, Double), (Double, Double)), Double)
move = proc (entity, startTime, start, end, speed) -> do
	timeNow <- time -< ()
	let (curve, curveLength) = makePath radiusOfCurvature start end
	let s = ((timeNow-startTime)/(curveLength)) * speed -- * speed
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
	isCloseToTarget <- id -< magnitude ((fst $ actionFinishPosition action) - (target^.entityData.shipLocation)) > 50
	isActionDone <- finishedAction -< (game, entity, action)
	isPlanOutdated <- arr' (||) -< (isCloseToTarget, isActionDone)
	if isPlanOutdated 
		then id -< [UpdateShipPlan (entity^.entityID) []]
		else move -< (entity, actionStartTime action, actionStartPosition action, actionFinishPosition action, shipSpeed' game entity)

	-- if magnitude ((fst $ actionFinishPosition action) - (target^.entityData.shipLocation)) > 50
	-- 	|| finishedAction game (entity^.entityData) action
	-- 	then id -< [UpdateShipPlan (entity^.entityID) []]
	-- 	else move -< (entity, actionStartTime action, actionStartPosition action, actionFinishPosition action, shipSpeed' game entity)
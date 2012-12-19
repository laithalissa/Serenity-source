{-# LANGUAGE Arrows #-}

module Serenity.Game.Server.GameStateTransform 
(	Command(..)
,	Update(..)
,	GameState(gameStateEntities)
,	ShipOrder(..)
,	ShipOrderState(..)
,	Entity(..)
,	GameEntity(..)
,	transform
,	transforms
,	step
,	nextLocation
) where

import Prelude hiding ((.), id)
import Control.Wire
import Control.Monad.Identity (Identity, runIdentity)
import GHC.Float

import Serenity.Network.Message (Command(..), Update(..))
--import Serenity.Maths.Vector

import Serenity.Game.Shared.Model.GameState (GameState(..), GameMap(..), hasEntityId, getEntityById)
import Serenity.Game.Shared.Model.Common    (TimeDuration)
import Serenity.Game.Shared.Model.ShipOrder (ShipOrder(..), ShipOrderState(..))
import Serenity.Game.Shared.Model.Entity    (Entity(..), GameEntity(..))
import Serenity.Game.Shared.Model.Common    (Direction, Speed, Location)

import Serenity.Game.Server.EntityController (entityUpdateSelf, shipNewOrder)

import qualified Data.Set as Set
import Data.Maybe(fromJust)

-- | gives the command to the relevent ship, returning the changes to the world
transform 
	:: Command   -- ^ ship command
	-> GameState -- ^ current game
	-> [Update]  -- ^ updates to current game 
transform (GiveOrder orderEntityId order) gameState =
	if hasEntityId orderEntityId gameState 
		then [UpdateEntity $ shipNewOrder gameState order (fromJust $ getEntityById orderEntityId gameState)]
		else []

-- | similar to transform, except it applies multiple commands to the given gamestate
transforms 
	:: [Command] -- ^ ship commands
	-> GameState -- ^ current game
	-> [Update]  -- ^ updates to current game
transforms commands gameState = concatMap ((flip transform) gameState) commands

-- | each entity updates its current shipSpeed, then next location is calculated
step' :: TimeDuration -> GameState -> [Update]
step' td gs = (map (UpdateEntity . wrap (stepEntity td) . (entityUpdateSelf gs)) . Set.toList . gameStateEntities) gs

type BaseWire = Wire () Identity
type UpdateWire = BaseWire GameState [Update]

updateOnTime :: UpdateWire -> Float -> GameState -> ([Update], UpdateWire)
updateOnTime wire timeDelta gameState = (updates result, newWire) where 
	(result, newWire) = runIdentity $ stepWire wire (float2Double timeDelta) gameState
	updates (Right u) = u
	updates (Left _ ) = []

topWire :: UpdateWire
topWire = mkPure (\time gamestate -> (Right $ step' (double2Float time) gamestate, topWire))

step'' = updateOnTime topWire
step time gameState = fst $ step'' time gameState

---------- private functions ----------


		
wrap :: (Entity -> Entity) -> GameEntity -> GameEntity
wrap f ge = ge{entity= (f . entity) ge}	


-- entityMakeMove :: Entity -> Entity
-- entityMakeMove entity@(Ship{shipLocation=(x,y),shipSpeed=(dx,dy), shipOrder=order}) =
-- 	case order of
-- 		StayStillOrder -> entity{shipSpeed=(0,0), shipDirection=(0,1)}
-- 		MoveOrder{moveOrderPath=mPath} -> 
-- 			if (distance (x,y) (fst $ target mPath, snd $ target mPath)) < 10
-- 				then entityMakeMove entity{shipOrder=StayStillOrder}
-- 				else
-- 					entity
-- 					{	shipSpeed = normalized (tx-x, ty-y)
-- 					,	shipDirection = normalized (tx-x, ty-y)
-- 					}
-- 		where
-- 		target = head . fromJust
-- entityMakeMove entity = entity


stepEntity :: TimeDuration -> Entity -> Entity
stepEntity tD entity@(Ship{ shipLocation=location, shipSpeed=speed }) =
	entity
	{	shipLocation = nextLocation location speed tD 
	,	shipDirection = if speed == (0,0) then shipDirection entity else speed
	}
	where
		speed = shipSpeed entity
stepEntity tD entity = entity


nextLocation :: Location -> Speed -> TimeDuration -> Location
nextLocation (x,y) (dx, dy) dt = (x + dx * dt, y + dy * dt)


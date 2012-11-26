
module Serenity.Game.Server.GameStateTransform 
(	Command(..)
,	Update(..)
,	GameState(gameStateEntities)
,	ShipOrder(..)
,	Entity(..)
,	GameEntity(..)
,	transform
,	transforms
,	step
,	nextLocation
) where

import Serenity.Network.Message
	(	Command(..)
	,	Update(..)
	)

import Serenity.Game.Shared.Model.GameState
	(	GameState(..)
	,	hasEntityId
	,	getEntityById
	)

import Serenity.Game.Shared.Model.Common(TimeDuration)

import Serenity.Game.Shared.Model.ShipOrder
	(	ShipOrder(..)
	)

import Serenity.Network.Message
	(	Update(..)
	,	Command(..)
	)

import Serenity.Game.Shared.Model.Entity
	(	Entity(..)
	,	GameEntity(..)
	)

import Serenity.Game.Shared.Model.Common(Direction, Speed, Location)

import qualified Data.Set as Set
import Data.Maybe(fromJust)

-- | gives the command to the relevent ship, returning the changes to the world
transform :: Command  -- ^ ship command
	-> GameState -- ^ current game
	-> [Update] -- ^ updates to current game 
transform (GiveOrder orderEntityId order) gameState =
	if hasEntityId orderEntityId gameState 
		then [UpdateEntity $ giveOrder (fromJust $ getEntityById orderEntityId gameState) order]
		else []	

-- | similar to transform, except it applies multiple commands to the given gamestate
transforms :: [Command] -- ^ ship commands
	-> GameState -- ^ current game
	-> [Update] -- ^ updates to current game
transforms commands gameState = concatMap ((flip transform) gameState) commands

	
-- | applies the ship order to the given GameEntity.
giveOrder :: GameEntity -- ^ current game entity
	-> ShipOrder  -- ^ ship order to apply to game entity
	-> GameEntity -- ^ game entity after order applied
giveOrder gameEntity shipOrder = wrap (mutateEntity shipOrder) gameEntity


mutateEntity :: ShipOrder -> Entity -> Entity
mutateEntity order ship@Ship{} = ship{shipOrder=order}
mutateEntity entity order = entity
		

wrap :: (Entity -> Entity) -> GameEntity -> GameEntity
wrap f ge = ge{entity= (f . entity) ge}	

-- | each entity updates its current shipSpeed, then next location is calculated
step :: TimeDuration -> GameState -> [Update]
step td = map (UpdateEntity . wrap (stepEntity td . entityMakeMove)) . Set.toList . gameStateEntities



entityMakeMove :: Entity -> Entity
entityMakeMove entity@(Ship{shipLocation=(x,y),shipSpeed=(dx,dy), shipOrder=order}) =
	case order of
		StayStillOrder -> entity{shipSpeed=(0,0), shipDirection=(0,1)}
		MoveOrder (tx,ty) -> entity
			{	shipSpeed = unitVector (tx-x, ty-y)
			,	shipDirection = unitVector (tx-x, ty-y)
			}
entityMakeMove entity = entity


stepEntity :: TimeDuration -> Entity -> Entity
stepEntity tD entity@(Ship{ shipLocation=location, shipSpeed=speed }) =
	entity
	{	shipLocation = nextLocation location speed tD 
	,	shipDirection = shipSpeed entity
	}
stepEntity tD entity = entity


nextLocation :: Location -> Speed -> TimeDuration -> Location
nextLocation (x,y) (dx, dy) dt = (x + dx * dt, y + dy * dt)


unitVector :: (Float, Float) -> (Float, Float)
unitVector (x,y) = (x/magnitude, y/magnitude)
	where
		magnitude = (x**2 + y**2)**0.5



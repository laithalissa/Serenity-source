
module Serenity.Game.Server.GameStateTransform 
(	Command(..)
,	Update(..)
,	GameState(gameStateEntities)
,	ShipOrder(..)
,	Entity(..)
,	GameEntity(..)
,	transform
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

transform :: Command -> GameState -> [Update]
transform (GiveOrder orderEntityId order) gameState =
	if hasEntityId orderEntityId gameState 
		then [UpdateEntity $ giveOrder (fromJust $ getEntityById orderEntityId gameState) order]
		else []	


giveOrder :: GameEntity -> ShipOrder -> GameEntity
giveOrder gameEntity shipOrder = gameEntity{entity=mutateEntity (entity gameEntity) shipOrder}


mutateEntity :: Entity -> ShipOrder -> Entity
mutateEntity ship@Ship{} order = ship{shipOrder=order}
mutateEntity entity order = entity
		
	

-- | each entity updates its current shipSpeed, then next location is calculated
step :: TimeDuration -> GameState -> [Update]
step td = map (UpdateEntity . wrap (stepEntity td . entityMakeMove)) . Set.toList . gameStateEntities
		where
		wrap :: (Entity -> Entity) -> GameEntity -> GameEntity
		wrap f ge = ge{entity= (f . entity) ge}


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
	entity{ shipLocation = nextLocation location speed tD }
stepEntity tD entity = entity


nextLocation :: Location -> Speed -> TimeDuration -> Location
nextLocation (x,y) (dx, dy) dt = (x + dx * dt, y + dy * dt)


unitVector :: (Float, Float) -> (Float, Float)
unitVector (x,y) = (x/magnitude, y/magnitude)
	where
		magnitude = (x**2 + y**2)**0.5
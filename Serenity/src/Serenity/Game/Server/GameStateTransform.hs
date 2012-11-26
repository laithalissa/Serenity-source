
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
	,	GameMap(..)
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



	-- client command -> updates
	-- 	- update entity with new command
	-- 	- entity builds any information for new command, may require current game state

	-- time -> updates
	-- 	- ships move to their location
	


-- | similar to transform, except it applies multiple commands to the given gamestate
transforms :: [Command] -- ^ ship commands
	-> GameState -- ^ current game
	-> [Update] -- ^ updates to current game
transforms commands gameState = concatMap ((flip transform) gameState) commands


-- | gives the command to the relevent ship, returning the changes to the world
transform :: Command  -- ^ ship command
	-> GameState -- ^ current game
	-> [Update] -- ^ updates to current game 
transform (GiveOrder orderEntityId order) gameState =
	if hasEntityId orderEntityId gameState 
		then [UpdateEntity $ giveOrder gameState (fromJust $ getEntityById orderEntityId gameState) order]
		else []	



-- | each entity updates its current shipSpeed, then next location is calculated
step :: TimeDuration -> GameState -> [Update]
step td = map (UpdateEntity . wrap (stepEntity td . entityMakeMove)) . Set.toList . gameStateEntities



---------- private functions ----------


-- | applies the ship order to the given GameEntity.
giveOrder :: GameState
	-> GameEntity -- ^ current game entity
	-> ShipOrder  -- ^ ship order to apply to game entity
	-> GameEntity -- ^ game entity after order applied
giveOrder gameState gameEntity shipOrder = wrap (giveShipOrder (gameStateGameMap gameState) shipOrder) gameEntity


giveShipOrder :: GameMap -> ShipOrder -> Entity -> Entity
giveShipOrder gameMap order ship = ship{shipOrder=order}
giveShipOrder gameMap order@MoveOrder{moveOrderLocation=target} ship@Ship{shipLocation=location} = ship{shipOrder=order{moveOrderPath=Just[(250,250), target]}}

		
wrap :: (Entity -> Entity) -> GameEntity -> GameEntity
wrap f ge = ge{entity= (f . entity) ge}	


entityMakeMove :: Entity -> Entity
entityMakeMove entity@(Ship{shipLocation=(x,y),shipSpeed=(dx,dy), shipOrder=order}) =
	case order of
		StayStillOrder -> entity{shipSpeed=(0,0), shipDirection=(0,1)}
		MoveOrder{moveOrderLocation=(tx,ty)}-> entity
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



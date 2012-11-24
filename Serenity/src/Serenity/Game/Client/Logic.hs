module Serenity.Game.Client.Logic
(	handleClick
)
where

import Data.Set (Set)
import qualified Data.Set as Set

import Serenity.Game.Client.ClientMessage (ClientMessage(ClientMessageCommand))

import Serenity.Game.Shared.Model.Common (OwnerId)
import Serenity.Game.Shared.Model.Entity (Entity(Ship), GameEntity(..))
import Serenity.Game.Shared.Model.GameState (GameState(..))
import Serenity.Game.Shared.Model.ShipOrder

import Serenity.Network.Message (Command(..))

handleClick :: (Float, Float) -> GameState -> OwnerId -> [ClientMessage]
handleClick click gameState player = case playersShips player (gameStateEntities gameState) of
	[] -> []
	ships -> map (\s -> ClientMessageCommand $ GiveOrder (entityId s) order) ships

	where order = MoveOrder click -- XXX need to convert to game coords

playersShips :: OwnerId -> Set GameEntity -> [GameEntity]
playersShips player entities = Set.toList $ Set.filter (playersShip player) entities
	where
		playersShip p (GameEntity { ownerId = owner, entity = entity }) =
			owner == p && isShip entity
		isShip (Ship {}) = True
		isShip _ = False

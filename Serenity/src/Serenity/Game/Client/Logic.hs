module Serenity.Game.Client.Logic
(	handleClick
)
where

import Data.Set (Set)
import qualified Data.Set as Set

import Serenity.Game.Client.ClientMessage (ClientMessage(ClientMessageCommand))
import Serenity.Game.Client.ClientState (ClientState(..), UIState(..), mapLocationFromView)

import Serenity.Game.Shared.Model.Common (OwnerId)
import Serenity.Game.Shared.Model.Entity (Entity(Ship), GameEntity(..))
import Serenity.Game.Shared.Model.GameMap (GameMap(..))
import Serenity.Game.Shared.Model.GameState (GameState(..))
import Serenity.Game.Shared.Model.ShipOrder

import Debug.Trace(trace)

import Serenity.Network.Message (Command(..))

handleClick :: (Float, Float) -> ClientState -> [ClientMessage]
handleClick click clientState = case playersShips (clientName clientState) entities of
	[] -> []
	ships -> {-trace ("Click: " ++ (show click) ++ "\n Mappedto: " ++ (show order)) $-} map (\s -> ClientMessageCommand $ GiveOrder (entityId s) order) ships

	where
		entities = gameStateEntities $ gameState clientState
		viewport = viewPort $ uiState clientState
		mapSize = gameMapSize $ gameStateGameMap $ gameState clientState
		order = MoveOrder (mapLocationFromView click viewport mapSize)

playersShips :: OwnerId -> Set GameEntity -> [GameEntity]
playersShips player entities = Set.toList $ Set.filter (playersShip player) entities
	where
		playersShip p (GameEntity { ownerId = owner, entity = entity }) =
			owner == p && isShip entity
		isShip (Ship {}) = True
		isShip _ = False

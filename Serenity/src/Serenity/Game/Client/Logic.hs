module Serenity.Game.Client.Logic
(	handleClick
)
where

import Serenity.Model
import Serenity.Game.Client.ClientMessage (ClientMessage(ClientMessageCommand))
import Serenity.Game.Client.ClientState

import Control.Lens
import qualified Data.Map as Map

handleClick :: (Double, Double) -> ClientState -> [ClientMessage]
handleClick click clientState = map f (playersShips clientState) 
	where
	f entity = ClientMessageCommand $ GiveOrder (entity^.entityID) order
	order = makeOrderMove orderLocation
	orderLocation = ( 
		mapLocationFromView click 
		(clientState^.clientUIState.viewport) 
		(clientState^.clientGame.gameBuilder.gbSector.sectorSize)
		)

playersShips :: ClientState -> [Entity Ship]
playersShips clientState = Map.elems $ Map.filter f $ clientState^.clientGame.gameShips where
	f entity = entity^.ownerID == clientState^.clientOwnerID
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

handleClick click clientState = if clickInSector then map f (playersShips clientState) else [] where
	f entity = ClientMessageCommand $ GiveOrder (entity^.entityID) order

	clickMapped = mapLocationFromView click (clientState^.clientUIState.uiStateViewport) (sX, sY)

	(sX,sY) = clientState^.clientGame.gameBuilder.gbSector.sectorSize
	(cX,cY) = clickMapped

	clickInSector = and [cX > 0, cY > 0, cX < sX, cY < sY]

	order = OrderMove clickMapped Nothing

playersShips :: ClientState -> [Entity Ship]
playersShips clientState = Map.elems $ Map.filter f $ clientState^.clientGame.gameShips where
	f entity = entity^.ownerID == clientState^.clientOwnerID
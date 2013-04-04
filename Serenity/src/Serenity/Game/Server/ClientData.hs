module Serenity.Game.Server.ClientData
(	ClientData(..)
)
where

import Serenity.Model.Fleet (Fleet)
import Serenity.Network.Transport (TransportInterface)

data ClientData = ClientData
	{	clientID :: Int
	,	clientName :: String
	,	clientTransportInterface :: TransportInterface
	,	clientFleet :: Fleet
	}

module Serenity.Game.Server.ClientData
(	ClientData(..)
)
where

import Serenity.Network.Server (TransportInterface)

data ClientData = ClientData
	{	clientTransportInterface :: TransportInterface
	}

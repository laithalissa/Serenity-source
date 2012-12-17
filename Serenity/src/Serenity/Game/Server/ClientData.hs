module Serenity.Game.Server.ClientData
(	ClientData(..)
)
where

import Serenity.Network.Transport (TransportInterface)

data ClientData = ClientData
	{	clientTransportInterface :: TransportInterface
	}

module Serenity.Game.Server.ClientData
(	ClientData(..)
)
where

import Serenity.Network.Utility

data ClientData = ClientData
	{	clientTransportInterface :: TransportInterface
	}
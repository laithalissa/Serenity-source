{-# LANGUAGE TemplateHaskell #-}

module Serenity.Network.Message 
(	Message (..)
,	Update(..)
,	Command(..)
,	Entity(..)
,	Ship_(..)
,	ShipOrder(..)
,	ShipOrderState(..)
)
where

import Serenity.Game.Shared.Model.Entity
import Serenity.Model.Entity

import Serenity.Extensions.Vinyl
import Data.Binary
import Data.DeriveTH

type ClientId = Int
type Time = Int

$(makeBinaryNewtype ''Ship "ship_" "Ship_")

-- | A message that can be sent over the network.
data Message = 
	  UpdateMessage Update Time            -- ^ Updates are messages containing new GameState information to be sent from the server to the clients.
	| CommandMessage Command ClientId Time -- ^ Commands are intention notifications sent from a specific client to the server.
	| Empty                                -- ^ An empty message (for networking and testing purposes).
	deriving (Show, Eq)

-- | Updates are sent from server to client and are used to update the GameState information on the clients. 
--   Any updates should be identically sent to all clients
data Update = 
	UpdateEntity
	{	updateEntity :: Ship_
	}
	| AddEntity
	{	updateEntity :: Ship_
	}
	| DeleteEntity
	{	updateEntity :: Ship_
	}
	| UpdateEntityLocation
	{	updateEntityID :: Int
	,	updateEntityLocation :: (Float, Float)
	}
	deriving (Show, Eq)

-- | Commands are sent from the clients to the server and contain order information and other notifications of intention.
data Command = 
	GiveOrder
	{	commandEntityId :: Int
	,	order :: Order
	}
	deriving (Show, Eq)

-- Derive binary instances using deep magic.
-- $(derive makeBinary ''ShipOrder)
-- $(derive makeBinary ''ShipOrderState)
-- $(derive makeBinary ''Entity)

$(derive makeBinary ''Order)
$(derive makeBinary ''Update)
$(derive makeBinary ''Command)
$(derive makeBinary ''Message)

{-# LANGUAGE TemplateHaskell #-}

module Serenity.Network.Message 
(	Message (..)
,	Update(..)
,	Command(..)
,	Entity(..)
,	ShipOrder(..)
)
where

import Serenity.Game.Shared.Model.Entity

import Data.Binary
import Data.DeriveTH

type ClientId = Int
type Time = Int

-- | A message that can be sent over the network.
data Message = 
	  UpdateMessage Update Time            -- ^ Updates are messages containing new GameState information to be sent from the server to the clients.
	| CommandMessage Command ClientId Time -- ^ Commands are intention notifications sent from a specific client to the server.
	| Empty                                -- ^ An empty message (for testing purposes).
	deriving (Show, Eq)

-- | Updates are sent from server to client and are used to update the GameState information on the clients. 
--   Any updates should be identically sent to all clients
data Update = 
	UpdateEntity
	{	entity :: Entity
	}
	| AddEntity
	{	entity :: Entity
	}
	| DeleteEntity
	{	entity :: Entity
	}
	deriving (Show, Eq)

-- | Commands are sent from the clients to the server and contain order information and other notifications of intention.
data Command = 
	GiveOrder
	{	entityId :: Int
	,	order :: ShipOrder
	}
	deriving (Show, Eq)

-- Derive binary instances using deep magic.
$(derive makeBinary ''ShipOrder)
$(derive makeBinary ''Entity)
$(derive makeBinary ''Update)
$(derive makeBinary ''Command)
$(derive makeBinary ''Message)
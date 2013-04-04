{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Message 
(	Message(..)
,	Update(..)
,	Command(..)
,	CtlMsg(..)
,	Entity(..)
,	Ship(..)
)
where

import Serenity.Model.Entity
import Serenity.Model.Fleet
import Serenity.Model.Sector

import Data.Binary
import Data.DeriveTH
import Data.Map (Map)

type ClientID = Int
type Time = Int

-- | A message that can be sent over the network.
data Message = 
	  UpdateMessage Update Time            -- ^ Updates are messages containing GameState information to be sent from the server to the clients.
	| CommandMessage Command ClientID Time -- ^ Commands are intention notifications sent from a specific client to the server.
	| ControlMessage CtlMsg
	| Empty                                -- ^ An empty message (for networking and testing purposes).
	deriving (Show, Eq)

-- | Updates are sent from server to client and are used to update the GameState information on the clients. 
--   Any updates should be identically sent to all clients
data Update = 
	UpdateEntity
	{	updateEntity :: Entity Ship
	}
	| AddEntity
	{	updateEntity :: Entity Ship
	}
	| DeleteEntity
	{	updateEntityID :: EntityID
	}
	| UpdateEntityLocation
	{	updateEntityID :: EntityID
	,	updateEntityLocation :: (Float, Float)
	}
	| UpdateEntityDirection
	{	updateEntityID :: EntityID
	,	updateEntityDirection :: (Float, Float)
	}
	| UpdateEntityLocationDirection
	{	updateEntityID :: EntityID
	,	updateEntityLocation :: (Float, Float)
	,	updateEntityDirection :: (Float, Float)
	}
	| UpdateShipOrder
	{	updateEntityID :: EntityID
	,	updateShipOrder :: Order
	}
	| UpdateShipPlan
	{	updateEntityID :: EntityID
	,	updateShipPlan :: Plan
	}
	| UpdateShipGoal
	{	updateEntityID :: EntityID
	,	updateShipGoal :: Goal
	}
	|	UpdateShipTargets
	{	updateEntityID :: EntityID
	,	updateShipTargets :: Map Int [EntityID]
	}
	| UpdateShipDamage
	{	updateEntityID :: EntityID
	,	updateShipDamage :: Damage
	}
	|	UpdateGameRanks
	{	updateGameRanks :: [(OwnerID, Int)]
	}
	| UpdateGameOver
	deriving (Show, Eq)

-- | Commands are sent from the clients to the server and contain order information and other notifications of intention.
data Command = 
	GiveOrder
	{	commandEntityID :: EntityID
	,	order :: Order
	}
	deriving (Show, Eq)

data CtlMsg =
	ControlSetName
	{	controlName :: String
	}
	| ControlYourID
	{	controlID :: Int
	}
	|	ControlSetConnected
	{	controlConnected :: [(Int, String)]
	}
	|	ControlReady
	|	ControlStarting
	deriving (Show, Eq)

-- Derive binary instances using deep magic.
-- $(derive makeBinary ''ShipOrder)
-- $(derive makeBinary ''ShipOrderState)
derive makeBinary ''Damage
derive makeBinary ''Entity
derive makeBinary ''ShipConfiguration
derive makeBinary ''System
derive makeBinary ''Weapon
derive makeBinary ''ShipClass
derive makeBinary ''WeaponSlot
derive makeBinary ''SystemSlot
derive makeBinary ''SlotType
derive makeBinary ''Resources
derive makeBinary ''WeaponEffect
derive makeBinary ''Ship
derive makeBinary ''Goal
derive makeBinary ''ShipAction
derive makeBinary ''Fleet

derive makeBinary ''Order
derive makeBinary ''Update
derive makeBinary ''Command
derive makeBinary ''CtlMsg
derive makeBinary ''Message

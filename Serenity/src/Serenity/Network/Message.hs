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

import Data.Word (Word32, Word16)

import Data.Binary
import Data.DeriveTH

type ClientId = Int

data Message = 
	  UpdateMessage Update
	| CommandMessage Command ClientId
	| Empty
	deriving (Show, Eq)

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

data Command = 
	GiveOrder
	{	entityId :: Int
	,	order :: ShipOrder
	}
	deriving (Show, Eq)

-- More Magic
$(derive makeBinary ''ShipOrder)
$(derive makeBinary ''Entity)
$(derive makeBinary ''Update)
$(derive makeBinary ''Command)
$(derive makeBinary ''Message)
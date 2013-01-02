{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Message where

import Serenity.Model.Game
import Serenity.Model.Time

import Data.Binary
import Serenity.Extensions.Vinyl
import Data.DeriveTH

type ClientId = Int
type MessageTime = Int

-- | A message that can be sent over the network.
data Message = 
	  UpdateMessage Update MessageTime            -- ^ Updates are messages containing new GameState information to be sent from the server to the clients.
	| CommandMessage Command ClientId MessageTime -- ^ Commands are intention notifications sent from a specific client to the server.
	| Empty                                -- ^ An empty message (for networking and testing purposes).

-- Derive binary instances using deep magic.
$(derive makeBinary ''Message)

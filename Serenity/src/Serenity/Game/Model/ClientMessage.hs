
module Serenity.Game.Model.ClientMessage where

import Serenity.Game.Model.Entity

data ClientCommand = 
     EmptyCommand |
     CreateEntityCommand Entity
     deriving(Show)

data ClientMessage = ClientMessage {
     clientId :: String,
     command :: ClientCommand                
}

data WorldDelta = 
     EntityAdded String |
     EntityDeleted Int deriving(Show)



     
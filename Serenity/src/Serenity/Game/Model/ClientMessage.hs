
module Serenity.Game.Model.ClientMessage where

import Serenity.Game.Model.Entity
import Serenity.Game.Model.Common

data ClientCommand = 
     MoveShip { shipId :: Int, target :: Location }
     deriving(Show)

data ClientMessage = ClientMessage {
     clientId :: String,
     command :: ClientCommand                
}

data WorldDelta = 
     EntityAdded String |
     EntityDeleted Int deriving(Show)



     
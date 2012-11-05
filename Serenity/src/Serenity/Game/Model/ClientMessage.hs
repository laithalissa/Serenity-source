
module Serenity.Game.Model.ClientMessage where


data ClientCommand = 
     EmptyCommand |
     CreateEntityCommand Entity
     deriving(Show)

data ClientMessage = ClientMessage {
     clientId :: String,
     command :: ClientCommand                
}

data ClientMessage = ClientMessage {
      
}

data WorldDelta = 
     EntityAdded String |
     EntityDeleted Int deriving(Show)



     
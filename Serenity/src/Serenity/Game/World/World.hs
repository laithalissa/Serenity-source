
module Serenity.Game.World.World where

data Entity = Entity { 
              entityName :: String,
              entityOwner :: String,
              entityLocation :: (Int, Int)
              } deriving(Show, Ord, Eq)


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

class World a where
      updateFromMessage :: ClientMessage -> a -> a
      updateAfterDuration :: Int -> a -> a
      takeAllDeltas :: a -> ([WorldDelta], a)
      
      updateCycle :: [ClientMessage] -> Int -> a -> a
      updateCycle messages timeDelta world =
          updateAfterDuration timeDelta $ foldl (flip updateFromMessage) world messages

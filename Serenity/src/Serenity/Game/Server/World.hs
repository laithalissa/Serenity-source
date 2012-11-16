
module Serenity.Game.Server.World where

--import Serenity.Game.Model.Common(Location)
import Serenity.Game.Model.ClientMessage(ClientMessage)
import Serenity.Game.Model.WorldDelta(WorldDelta)
import Serenity.Game.Model.GameMap(GameMap)


class World a where

      initialize :: GameMap -> a
      updateFromTimeDelta :: Int -> a -> a
      updateFromClient :: ClientMessage -> a -> a
      takeAllDeltas :: a -> ([WorldDelta], a)
      
      updateCycle :: [ClientMessage] -> Int -> a -> ([WorldDelta], a)
      updateCycle messages timeDelta world = 
          (takeAllDeltas . updateFromTimeDelta timeDelta . foldl (flip updateFromClient) world) messages


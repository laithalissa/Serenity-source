
module Serenity.Game.Server.World where

import Serenity.Game.World.World

class World a where
      updateFromMessage :: ClientMessage -> a -> a
      updateFromTimeDelta :: Int -> a -> a
      updateFromWorldDelta :: WorldDelta -> a -> a
      takeAllDeltas :: a -> ([WorldDelta], a)
      
      updateCycle :: [ClientMessage] -> Int -> a -> a
      updateCycle messages timeDelta world =
          updateAfterDuration timeDelta $ foldl (flip updateFromMessage) world messages



module Serenity.Game.Server.GameRunner where

import qualified Data.Set as Set

import Serenity.Game.World.World


main :: IO ()
main = let messages = initMessages
           world = initWorld
       in  (print . fst . takeAllDeltas . updateCycle messages 0) world     
    


initMessages = 
             [ ClientMessage "Joseph" EmptyCommand
             , ClientMessage "Joseph" (CreateEntityCommand $ Entity "Soldier" "Joseph" (10,15)) 
             ]

initWorld = SimpleWorld { 
          clients = Set.fromList ["Joseph", "Laith"], 
          entities = Set.fromList [],
          deltas = []
          }



data SimpleWorld = SimpleWorld { 
                   clients :: Set.Set String,
                   entities :: Set.Set Entity,
                   deltas :: [WorldDelta]
                   }


instance World SimpleWorld where
         updateFromMessage (ClientMessage _ cmd) world = case cmd of
             EmptyCommand -> world
             CreateEntityCommand entity -> SimpleWorld {
                                          clients=(clients world),
                                          entities=(Set.insert entity (entities world)),
                                          deltas=((deltas world) ++ [EntityAdded (show entity)])
                                          }

         updateAfterDuration timeDelta world = world

         takeAllDeltas (SimpleWorld clients entities deltas) = 
             (deltas, SimpleWorld clients entities [])                                
                                



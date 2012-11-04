
module Serenity.Game.Server.GameRunner where

import qualified Data.Set as Set

<<<<<<< HEAD
import qualified Serenity.Game.Server.PrototypeWorld as PW
import qualified Serenity.Game.Server.World as W
import Serenity.Game.Model.ClientMessage
import Serenity.Game.Model.Common
import Serenity.Game.Model.Entity
import Serenity.Game.Model.GameMap
=======
import Serenity.Game.World.World
>>>>>>> developed the game loop concept, defined in the GameRunner module. updated Network module to provide a helper fuction to get the adddress of the client. added a World package which wiill contain the model of the world used by both the client and server.  added Module World which has the minimal definitions to define the world. world is defined in terms of a class, in which no implementation is providied in that module.


main :: IO ()
main = let messages = initMessages
<<<<<<< HEAD
           world = W.initialize gameMap :: PW.PrototypeWorld
       in  (print . fst . W.updateCycle messages 0) world     
=======
           world = initWorld
       in  (print . fst . takeAllDeltas . updateCycle messages 0) world     
>>>>>>> developed the game loop concept, defined in the GameRunner module. updated Network module to provide a helper fuction to get the adddress of the client. added a World package which wiill contain the model of the world used by both the client and server.  added Module World which has the minimal definitions to define the world. world is defined in terms of a class, in which no implementation is providied in that module.
    


initMessages = 
             [ ClientMessage "Joseph" EmptyCommand
             , ClientMessage "Joseph" (CreateEntityCommand $ Entity "Soldier" "Joseph" (10,15)) 
             ]

<<<<<<< HEAD
gameMap = GameMap {
            name = "My First Map",
            size = (100, 100),
            spawnPoints=[(50, 50)],
            planets = [Planet {
                         planetName="Earth", 
                         planetType="HUGE", 
                         location=(50,50), 
                         direction=(0,1), 
                         resources=Resources {
                                      fuel=10,
                                      antiMatter=20,
                                      metal=30}}],
            spaceLanes=[]}
=======
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
                                

>>>>>>> developed the game loop concept, defined in the GameRunner module. updated Network module to provide a helper fuction to get the adddress of the client. added a World package which wiill contain the model of the world used by both the client and server.  added Module World which has the minimal definitions to define the world. world is defined in terms of a class, in which no implementation is providied in that module.


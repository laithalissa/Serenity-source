
module Serenity.Game.Server.GameRunner where

import qualified Data.Set as Set

import qualified Serenity.Game.Server.PrototypeWorld as PW
import qualified Serenity.Game.Server.World as W
import Serenity.Game.Model.ClientMessage
import Serenity.Game.Model.Common
import Serenity.Game.Model.Entity
import Serenity.Game.Model.GameMap

main :: IO ()
main = let messages = initMessages
           world = W.initialize gameMap :: PW.PrototypeWorld
       in  (print . fst . W.updateCycle messages 0) world     
    


initMessages = 
             [ ClientMessage "Joseph" EmptyCommand
             , ClientMessage "Joseph" (CreateEntityCommand $ Entity "Soldier" "Joseph" (10,15)) 
             ]

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



module Serenity.Game.Server.GameRunner where

import qualified Data.Set as Set

import qualified Serenity.Game.Server.World as W
import Serenity.Game.Model.ClientMessage
import Serenity.Game.Model.Common
import qualified Serenity.Game.Model.Entity as E
import Serenity.Game.Model.GameMap
import qualified Serenity.Game.Server.Demo as D

main :: IO ()
main = D.main

-- main = let messages = initMessages
--            world = W.initialize gameMap :: PW.PrototypeWorld
--        in  (print . snd . W.updateCycle messages 0) world     


-- initMessages = 
--              [ ClientMessage {  clientId="Joseph"
--                              ,  command=MoveShip 0 (25, 25) 
--                              }
--              ]

-- gameMap = GameMap {
--             name = "My First Map",
--             size = (100, 100),
--             spawnPoints=[(50, 50)],
--             planets = [Planet {
--                          planetName="Earth", 
--                          planetType="HUGE", 
--                          location=(50,50), 
--                          direction=(0,1), 
--                          resources=Resources {
--                                       fuel=10,
--                                       antiMatter=20,
--                                       metal=30}}],
--             spaceLanes=[]}


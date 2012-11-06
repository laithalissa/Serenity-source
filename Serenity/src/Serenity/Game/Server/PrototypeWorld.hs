
module Serenity.Game.Server.PrototypeWorld where

import Serenity.Game.Server.World(World(..))

import Serenity.Game.Model.ClientMessage()
import qualified Serenity.Game.Model.GameMap as GM


data PrototypeWorld = PrototypeWorld {
     name :: String
} deriving(Show, Eq)

instance World PrototypeWorld where
         initialize gameMap = PrototypeWorld (GM.name gameMap)
         updateFromTimeDelta time world = world
         updateFromClient message world = world
         takeAllDeltas world = ([], world)


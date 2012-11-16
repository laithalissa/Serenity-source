
module Serenity.Game.Server.PrototypeWorld where

import Serenity.Game.Server.World(World(..))

import Serenity.Game.Model.ClientMessage()
import qualified Serenity.Game.Model.GameMap as GM
import qualified Serenity.Game.Model.Entity as E
import qualified Serenity.Game.Model.ClientMessage as CM
                                         

data PrototypeWorld = PrototypeWorld {
     gameMap :: GM.GameMap,
     entities :: [E.Entity]
} deriving(Show, Eq)

instance World PrototypeWorld where

    initialize gameMap = 
        PrototypeWorld{
            gameMap=gameMap,
            entities=[
                E.Ship { 
                    E.shipId=0, 
                    E.location=(50,50),
                    E.direction=(0,1),
                    E.speed=(1)
                }
            ]
        }



    updateFromTimeDelta time world = world




         
    updateFromClient message world = case (CM.command message) of
        (CM.MoveShip shipId target) -> world{ entities=map f (entities world) }
          where
            f entity = if (E.shipId entity) == shipId
                         then entity{E.location=target}
                         else entity


    takeAllDeltas world = ([], world)







module Serenity.Game.Server.PrototypeWorld where

import Serenity.Game.Server.World(World(..))

import Serenity.Game.Model.ClientMessage()
import qualified Serenity.Game.Model.GameMap as GM
import qualified Serenity.Game.Model.Entity as E
import qualified Serenity.Game.Model.ClientMessage as CM
import qualified Serenity.Game.Model.ShipOrder as SO
                                         

data PrototypeWorld = PrototypeWorld {
     gameMap :: GM.GameMap,
     entityManager :: EntityManager
} deriving(Show, Eq)

instance World PrototypeWorld where

    initialize gameMap = 
        PrototypeWorld{
            gameMap=gameMap,
            entityManager= EntityManager [createShip]
        }



    updateFromTimeDelta time world = world




         
    updateFromClient message world = case (CM.command message) of
        (CM.MoveShip shipId target) -> world{ 
                     entityManager = EntityManager (map f (entities (entityManager world)))
          }
          where
            f entity = if (E.shipId entity) == shipId
                         then entity{E.location=target}
                         else entity


    takeAllDeltas world = ([], world)



----- Entities -------------

createShip :: E.Entity
createShip = E.Ship {E.shipId=0, 
                     E.shipLocation=(10,10),
                     E.shipDirection=(0,1),
                     E.shipSpeed=1.5,
                     E.shipOrder=SO.NoOrder
                    }


----- Entity Manager ----------
    
data EntityManager = EntityManager {
  entities :: [E.Entity] 
} deriving(Eq, Ord, Show)                 
          



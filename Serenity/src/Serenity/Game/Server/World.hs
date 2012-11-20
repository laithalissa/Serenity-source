
module Serenity.Game.Server.World where

import Graphics.Gloss.Data.Picture(Picture)

import Serenity.Game.Model.ClientMessage(ClientMessage)
import Serenity.Game.Model.WorldDelta(WorldDelta)
import Serenity.Game.Model.GameMap(GameMap)
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.ShipClass(ShipClass(..))  

import qualified Data.Map as Map

data AssetManager = AssetManager
     {            assetManagerPictures :: Map.Map String Picture
     ,            assetManagerShipClasses :: Map.Map String ShipClass
     } deriving(Eq, Show)

class World a where
      initialize :: AssetManager -> (Int, Int) -> GameMap -> a
      updateFromTimeDelta :: TimeDuration -> a -> IO a
      updateFromCommand :: ClientMessage -> a -> IO a
      render :: a -> IO Picture


module Serenity.Game.Model.GameMap
( GameMap(..)
, Planet(..)
, SpaceLane(..)
) where

import Serenity.Game.Model.Common(Location, Direction, Resources, Size)


data GameMap = GameMap{ 
  name :: String
, size :: Size
, spawnPoints :: [(Location)]
, planets :: [Planet]
, spaceLanes :: [SpaceLane]
}

data Planet = Planet { 
  planetName :: String
, planetType :: String -- specifies which size / texture to use
, location :: Location
, direction :: Direction
, resources :: Resources
}

data SpaceLane = SpaceLane { 
  planetName1 :: String
, planetName2 :: String
}

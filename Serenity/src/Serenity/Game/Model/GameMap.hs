
module Serenity.Game.Model.GameMap
<<<<<<< HEAD
( GameMap(..)
, Planet(..)
, SpaceLane(..)
) where

import Serenity.Game.Model.Common(Location, Direction, Resources, Size)


data GameMap = GameMap
{ name :: String
, size :: Size
, spawnPoints :: [(Location)]
, planets :: [Planet]
, spaceLanes :: [SpaceLane]
}

data Planet = Planet 
{ id :: Int
, planetType :: String -- specifies which size / texture to use
, location :: Location
, direction :: Direction
, resources :: Resources
}

data SpaceLane = SpaceLane 
{ planetId1 :: Int
, planetId2 :: Int
}

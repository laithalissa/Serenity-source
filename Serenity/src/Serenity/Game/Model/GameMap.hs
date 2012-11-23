module Serenity.Game.Model.GameMap
(	GameMap(..)
,	Planet(..)
,	SpaceLane(..)
) where

import Serenity.Game.Model.Common(Location, Direction, Resources, Size)

data GameMap = GameMap 
	{	gameMapName :: String
	,	gameMapSize :: Size
	,	gameMapSpawnPoints :: [(Location)]
	,	gameMapPlanets :: [Planet]
	,	gameMapSpaceLanes :: [SpaceLane]
	}
	deriving(Show, Eq)

data Planet = Planet 
	{	planetName :: String
	,	planetType :: String -- specifies which size / texture to use
	,	planetLocation :: Location
	,	planetDirection :: Direction
	,	planetResources :: Resources
	}
	deriving(Show, Eq)

data SpaceLane = SpaceLane 
	{	spaceLanePlanet1 :: String
	,	spaceLanePlanet2 :: String
	}
	deriving(Show, Eq)

--data GameMap = GameMap
--	{	name :: String
--	,	size :: Size
--	,	spawnPoints :: [(Location)]
--	,	planets :: [Planet]
--	,	spaceLanes :: [SpaceLane]
--	} 
--deriving(Show, Eq)

--data Planet = Planet 
--	{	planetName :: String
--	,	planetType :: String -- specifies which size / texture to use
--	,	location :: Location
--	,	direction :: Direction
--	,	resources :: Resources
--	}
--	deriving(Show, Eq)

--data SpaceLane = SpaceLane 
--	{	planetName1 :: String
--	,	planetName2 :: String
--	}
--	deriving(Show, Eq)

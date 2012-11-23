module Serenity.Game.Model.GameMap
(	GameMap(..)
,	Planet(..)
,	SpaceLane(..)
,	exampleGameMap
) where

import Serenity.Game.Model.Common(Location, Direction, Resources(..), Size)

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

exampleGameMap = GameMap
	{	gameMapName = "My First Map"
	,	gameMapSize = (100, 100)
	,	gameMapSpawnPoints=[(50, 50)]
	,	gameMapPlanets =
			[	createPlanet "Earth" (50, 50)
			,	createPlanet "Mars" (10, 10)
			,	createPlanet "Pluto" (90, 10)
			]
	,	gameMapSpaceLanes=[SpaceLane "Earth" "Mars"]
	}
	where
		createPlanet name location = Planet
			{	planetName=name
			,	planetType="planet1"
			,	planetLocation=location
			,	planetDirection=(0,1)
			,	planetResources=Resources{fuel=10, antiMatter=10, metal=10}
			}

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

{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.Shared.Model.GameMap
(	GameMap(..)
,	Planet(..)
,	SpaceLane(..)
,	exampleGameMap
) where

import Serenity.Game.Shared.Model.Common(Location, Direction, Resources(..), Size)

data Planet = Planet
	{	planetName :: String
	,	planetType :: String -- specifies which size / texture to use
	,	planetLocation :: Location
	,	planetDirection :: Direction
	,	planetResources :: Resources
	}
	deriving(Show, Eq)



data GameMap = GameMap
	{	gameMapName :: String
	,	gameMapSize :: Size
	,	gameMapSpawnPoints :: [(Location)]
	,	gameMapPlanets :: [Planet]
	,	gameMapSpaceLanes :: [SpaceLane]
	}
	deriving(Show, Eq)


data SpaceLane = SpaceLane
	{	spaceLanePlanet1 :: String
	,	spaceLanePlanet2 :: String
	}
	deriving(Show, Eq)


exampleGameMap = GameMap
	{	gameMapName = "My First Map"
	,	gameMapSize = (500, 500)
	,	gameMapSpawnPoints=[(50, 50)]
	,	gameMapPlanets = 
			[	createPlanet "Jupitor" (50, 50)
			,	createPlanet "Mars" (450, 50)
			,	createPlanet "Pluto" (50, 450)
			,	createPlanet "Venus" (450, 450)
			,	createPlanet "Earth" (250, 250)
			,	createPlanet "Moon" (250, 200)
			]
	,	gameMapSpaceLanes=
			[	SpaceLane "Earth" "Jupitor"
			,	SpaceLane "Earth" "Mars"
			,	SpaceLane "Earth" "Pluto"
			,	SpaceLane "Earth" "Venus"
			,	SpaceLane "Earth" "Moon"
			]
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

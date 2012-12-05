{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.Shared.Model.GameMap
(	GameMap(..)
,	Planet(..)
,	SpaceLane(..)
,	exampleGameMap
,	demoGameMap
,	getPlanetLocations
,	getPlanetLocationByName
,	getPlanetNames
,	getConnectedPlanets
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

demoGameMap = GameMap
	{	gameMapName = "Map for progress demo"
	,	gameMapSize = (100, 100)
	,	gameMapSpawnPoints = []
	,	gameMapPlanets =
			[	createPlanet (70, 30)
			,	createPlanet (10, 10)
			,	createPlanet (90, 90)
			]
	,	gameMapSpaceLanes =
			[	createSpaceLane (70, 30) (90, 90)
			,	createSpaceLane (10, 10) (70, 30)
			]
	}
	where
		createPlanet location@(x,y) = Planet
			{	planetName="planet " ++ (show location)
			,	planetType="planet1"
			,	planetLocation=(fromIntegral x, fromIntegral y)
			,	planetDirection=(0,1)
			,	planetResources=Resources{fuel=10, antiMatter=10, metal=10}
			}
		createSpaceLane l1 l2 = SpaceLane ("planet " ++ (show l1)) ("planet " ++ (show l2))

exampleGameMap = GameMap
	{	gameMapName = "My First Map"
	,	gameMapSize = (500, 500)
	,	gameMapSpawnPoints=[(50, 50)]
	-- ,	gameMapPlanets = 
	-- 		[	createPlanet "Jupitor" (50, 50)
	-- 		,	createPlanet "Mars" (450, 50)
	-- 		,	createPlanet "Pluto" (50, 450)
	-- 		,	createPlanet "Venus" (450, 450)
	-- 		,	createPlanet "Earth" (250, 250)
	-- 		,	createPlanet "Moon" (250, 200)
	-- 		]
	-- ,	gameMapSpaceLanes=
	-- 		[	SpaceLane "Earth" "Jupitor"
	-- 		,	SpaceLane "Earth" "Mars"
	-- 		,	SpaceLane "Earth" "Pluto"
	-- 		,	SpaceLane "Earth" "Venus"
	-- 		,	SpaceLane "Earth" "Moon"
	-- 		]
	,	gameMapPlanets =
			[	cp (20, 480)
			,	cp (20, 250)
			,	cp (20, 20)
			
			,	cp (250, 365)
			,	cp (250, 135)

			,	cp (480, 480)
			,	cp (480, 250)
			,	cp (480, 20)
			]
	,	gameMapSpaceLanes=
			[	csl (20,480) (480, 480)
			,	csl (20, 480) (480, 20)

			,	csl (20, 250) (250, 365)
			,	csl (20, 250) (250, 135)

			,	csl (20, 20) (250, 135)

			,	csl (250, 365) (480, 250)

			,	csl (250, 135) (480, 20)

			,	csl (480, 480) (480, 250)

			,	csl (480, 250) (480, 20)
			]
	}
	where
		csl :: (Int, Int) -> (Int, Int) -> SpaceLane
		csl l1 l2 = SpaceLane ("planet " ++ (show l1)) ("planet " ++ (show l2))
	
		cp :: (Int, Int) -> Planet
		cp location@(x,y) = Planet
			{	planetName="planet " ++ (show location)
			,	planetType="planet1"
			,	planetLocation=(fromIntegral x, fromIntegral y)
			,	planetDirection=(0,1)
			,	planetResources=Resources{fuel=10, antiMatter=10, metal=10}
			}


getPlanetLocations :: GameMap -> [Location]
getPlanetLocations = map planetLocation . gameMapPlanets

getPlanetLocationByName :: String -> GameMap -> Location
getPlanetLocationByName name gameMap =  planetLocation $ getPlanetByName name gameMap

getPlanetNames :: GameMap -> [String]
getPlanetNames = map planetName . gameMapPlanets


getConnectedPlanets :: String -> GameMap -> [String]
getConnectedPlanets name gameMap = filter (isConnected gameMap name) allPlanetNames
	where
		allPlanetNames = getPlanetNames gameMap


getPlanetByName :: String -> GameMap -> Planet
getPlanetByName name = head . filter ((name==) . planetName) . gameMapPlanets



isConnected :: GameMap -> String -> String -> Bool
isConnected gameMap planet1 planet2 = not $ null (filter comp $ gameMapSpaceLanes gameMap)
	where
	comp (SpaceLane p1 p2) = ((planet1==p1)&&(planet2==p2)) || ((planet2==p1)&&(planet1==p2))



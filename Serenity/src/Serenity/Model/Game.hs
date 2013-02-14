{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game where

import Serenity.Debug(trace')

import Serenity.Model.Entity
import Serenity.Model.Fleet
import Serenity.Model.Sector

import Control.Lens
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map

data GameBuilder = GameBuilder
	{	_gbSector	:: Sector
	,	_gbShipClasses 	:: Map String ShipClass
	,	_gbWeapons 	:: Map String Weapon
	,	_gbSystems	:: Map String System
	,	_gbPlayerFleets	:: Map OwnerID Fleet
	}
	deriving(Show, Eq)
makeLenses ''GameBuilder

data Game = Game
	{	_gameTime :: Double
	,	_gameRandom :: StdGen
	,	_gameNextEntityId :: Int
	,	_gameShips  :: Map EntityID (Entity Ship)
	,	_gameBuidler :: GameBuilder
	}
	deriving Show
makeLenses ''Game

addShip :: OwnerID -> Ship -> Game -> Game
addShip ownerId ship game = game'
	where
	game' = gameNextEntityId .~ eId' $ (gameShips .~ ships' $ game)
	eId' = eId+1
	eId = game^.gameNextEntityId
	ships' = Map.insert eId entity' (game^.gameShips)
	entity' = Entity
		{	_entityID=eId
		,	_ownerID=ownerId
		,	_entityData=ship
		}

initGame :: GameBuilder -> Game
initGame gameBuilder = game'
	where 
	fleetsList :: [(OwnerID, Fleet)]
	fleetsList = Map.toList (gameBuilder^gbPlayerFleets)
	spawnPoints :: [(Double, Double)]
	spawnPoints = gameBuilder^.gbSector^.sectorSpawnPoints
	fleetsSpawnPoint :: [(OwnerID, Fleet, Double, Double)]
	fleetsSpawnPoint = zipWith (\(oId, f) (x,y) -> (oId, f, x, y)) fleetsList spawnPoints
	shipsSpawnPoint :: [(OwnerID, ShipConfiguration, Double, Double)]
	shipsSpawnPoint = concat $ map f fleetsSpawnPoint
		where
		f (oId,Fleet scs,x,y) = map (\sc-> (oId,sc,x,y)) scs
	game' :: Game
	game' = foldl f game shipsSpawnPoint
		where
		f g (oId,sc,x,y) = addShip oId (initShip sc (x,y) (0,1)) g
		
	game = Game
		{	_gameTime = 0
		,	_gameRandom = mkStdGen 1758836
		,	_gameNextEntityId=0
		,	_gameShips  = Map.empty
		,	_gameBuilder = gameBuilder
		}

demoGame :: GameBuilder -> Game
demoGame gameBuilder = game' game
	where 
	game = initGame gameBuilder
	fleets = Map.fromList 
		[	(0, demoFleet)
		,	(1, demoFleet)
		,	(2, demoFleet)
		,	(3, demoFleet)
		]
	game' game = gameShips .~ (Map.map f (game^.gameShips)) $ game
		where
		f :: Entity Ship -> Entity Ship
		f e = entityData.shipOrder .~ (OrderAttack $ (getEntity ((e^.ownerID + 1) `mod` 4))^.entityID) $ e
		getEntity :: OwnerID -> Entity Ship
		getEntity oId = foldl1 (\x y -> if x^.ownerID == oId then x else y) $ Map.elems (game^.gameShips)

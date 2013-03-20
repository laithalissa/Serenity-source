{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
module Serenity.Model.Game where

import Data.Maybe(fromJust)

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

data GameMode =
	DeathMatch
	deriving Show

data Game = Game
	{	_gameTime :: Double
	,	_gameRandom :: StdGen
	,	_gameNextEntityId :: Int
	,	_gameShips  :: Map EntityID (Entity Ship)
	,	_gameBuilder :: GameBuilder
	,	_gameGameMode :: GameMode
	,	_gamePlayers :: [(OwnerID, String)]
	,	_gameRanks :: [(OwnerID, Int)]
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

initGame :: [(OwnerID, String)] -> GameBuilder -> Game
initGame players gameBuilder = game'
	where 
	fleetsList :: [(OwnerID, Fleet)]
	fleetsList = Map.toList (gameBuilder^.gbPlayerFleets)
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
		,	_gameGameMode = DeathMatch
		,	_gamePlayers = players
		,	_gameRanks = []
		}



demoGame :: [(OwnerID, String)] -> GameBuilder -> Game
demoGame players gameBuilder = game
	where 
	game = initGame players gameBuilder
	game' game = gameShips .~ (Map.map f (game^.gameShips)) $ game
		where
		f :: Entity Ship -> Entity Ship
		--f e = entityData.shipOrder .~ (OrderAttack $ (getEntity ((e^.ownerID + 1) `mod` 4))^.entityID) $ e
		f = entityData.shipOrder .~ (makeOrderMove (190, 190))
		getEntity :: OwnerID -> Entity Ship
		getEntity oId = foldl1 (\x y -> if x^.ownerID == oId then x else y) $ Map.elems (game^.gameShips)




--shipClass' entity game = gameBuilder.gbShipClasses.(at $ entity^.entityData.shipConfiguration.shipConfigurationShipClass)

--shipCurrentHull :: Game -> EntityID -> Int

---------- Lens Helpers ----------

shipSpeed' :: Game -> Entity Ship -> Bool -> Double
shipSpeed' game entity isUsingSpaceLane = 
	let	multiplier = if isUsingSpaceLane then sectorSpaceLaneSpeed' game else 1.0
		shipSpeed = (shipClass' entity game)^.shipClassSpeed
	in	multiplier * shipSpeed


sectorSpaceLaneSpeed' :: Game -> Double
sectorSpaceLaneSpeed' game = game^.gameBuilder.gbSector.sectorSpaceLaneSpeedMultiplier

gameMap' :: Simple Lens Game Sector
gameMap' = gameBuilder.gbSector

shipClass' :: Entity Ship -> Game -> ShipClass
shipClass' entity game = 
	let
		shipClassName = entity^.entityData.shipConfiguration.shipConfigurationShipClass
	in
		fromJust $ Map.lookup shipClassName (game^.gameBuilder.gbShipClasses)

shipMaxHealth' :: Entity Ship -> Game -> Int
shipMaxHealth' entity game = (shipClass' entity game)^.shipClassMaxDamage.damageHull

shipCurrentDamage' :: Entity Ship -> Int
shipCurrentDamage' entity = entity^.entityData.shipDamage.damageHull

shipHealth' :: Entity Ship -> Game -> Int
shipHealth' entity game = (shipMaxHealth' entity game) - (shipCurrentDamage' entity)

gameEntity' :: EntityID -> Game -> Entity Ship
gameEntity' eID game = fromJust $ Map.lookup eID (game^.gameShips)

shipWeapons :: GameBuilder -> Getter Ship [(Maybe Weapon, WeaponSlot)]
shipWeapons gameBuilder = to (weaponInfo gameBuilder)
	where
		shipClass gB ship = fromJust $ Map.lookup (ship^.shipConfiguration.shipConfigurationShipClass) (gB^.gbShipClasses)
		weapons gB ship = map (fmap (\w -> fromJust $ Map.lookup w (gB^.gbWeapons))) (ship^.shipConfiguration.shipConfigurationWeapons)
		weaponInfo gameBuilder ship = zip (weapons gameBuilder ship) ((shipClass gameBuilder ship)^.shipClassWeaponSlots)

shipWeaponSlots :: GameBuilder -> Getter Ship [WeaponSlot]
shipWeaponSlots gameBuilder = to (weaponSlots gameBuilder)
	where
		shipClass gB ship = fromJust $ Map.lookup (ship^.shipConfiguration.shipConfigurationShipClass) (gB^.gbShipClasses)
		weaponSlots gB ship = (shipClass gB ship)^.shipClassWeaponSlots

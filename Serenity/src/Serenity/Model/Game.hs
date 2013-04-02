{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
module Serenity.Model.Game where


import Serenity.Model.Entity
import Serenity.Model.Fleet
import Serenity.Model.Formation
import Serenity.Model.Sector

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random

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
	| Unwinnable
	deriving Show

data Game = Game
	{	_gameTime :: Double
	,	_gameRandom :: StdGen
	,	_gameShips  :: Map EntityID (Entity Ship)
	,	_gameBuilder :: GameBuilder
	,	_gameGameMode :: GameMode
	,	_gamePlayers :: [(OwnerID, String)]
	,	_gameRanks :: [(OwnerID, Int)]
	}
	deriving Show
makeLenses ''Game

initGame :: [(OwnerID, String)] -> GameBuilder -> Game
initGame players gameBuilder = Game
	{	_gameTime = 0
	,	_gameRandom = mkStdGen 1758836
	,	_gameShips = Map.fromList ships
	,	_gameBuilder = gameBuilder
	,	_gameGameMode = DeathMatch
	,	_gamePlayers = players
	,	_gameRanks = []
	}
	where

	ships = map createShip $ zip [1..] $ concatMap expandFleet fleets
	createShip (eID, (oID, conf, loc)) = (eID, Entity eID oID (initShip conf loc (0, 1)))
	expandFleet (oID, fleet, loc) = zipWith (\conf loc' -> (oID, conf, loc')) (fleet^.fleetShips) (formation (length $ fleet^.fleetShips) 25 50 0 4 loc)

	fleets = zipWith (\(oID, fleet) loc -> (oID, fleet, loc)) fleetsList spawnPoints
	fleetsList = Map.toList (gameBuilder^.gbPlayerFleets)
	spawnPoints = gameBuilder^.gbSector.sectorSpawnPoints

---------- Lens Helpers ----------

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

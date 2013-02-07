{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Ship where

import Serenity.Model.Sector

import Control.Lens

data Ship = Ship
	{	_shipName :: String
	,	_shipLocation :: (Double, Double)
	,	_shipDirection :: (Double, Double)
	,	_shipDamage :: Damage
	,	_shipOrder :: Order
	,	_shipGoal :: Goal
	,	_shipPlan :: Plan
	}
	deriving (Show, Eq)

data Damage = Damage 
	{	_damageHull   :: Int
	,	_damageShield :: Int
	}
	deriving (Show, Eq)

----------------- Orders and Planning -------------------

data Order = 
	  OrderNone
	| OrderMove (Double, Double) (Maybe (Double, Double))
	| OrderAttack Int
	| OrderGuardShip Int
	| OrderGuardPlanet Int
	| OrderGuardLocation (Double, Double)
	| OrderCapture Int
	deriving (Show, Eq)

data Goal = 
	  GoalNone
	| GoalBeAt (Double, Double) (Maybe (Double, Double))
	| GoalDestroyed Int
	| GoalGuardShip Int
	| GoalGuardPlanet Int
	| GoalGuardLocation (Double, Double)
	| GoalCaptured Int
	deriving (Show, Eq)

data ShipAction = 
	ActionMove 
	{	startTime :: Double
	,	startLocDir :: ((Double,Double),(Double,Double))
	,	endLocDir   :: ((Double,Double),(Double,Double))
	}
	| ActionAttack {targetID :: Int}
	| ActionCapture Int
	deriving (Show, Eq)

type Plan = [ShipAction]

----------------- Weapons and Configuration -------------------

data ShipType = ShipType
	{	shipTypeFrontWeaponSlots   :: Int
	,	shipTypeSideWeaponSlots    :: Int
	,	shipTypeDorsalWeaponSlots  :: Int
	,	shipTypeSystemUpgradeSlots :: Int
	,	classMaxDamage :: Damage 
	}

data ShipConfiguration = ShipConfiguration 
	{	shipConfFrontWeapons   :: Weapon
	,	shipConfSideWeapons    :: Weapon
	,	shipConfDorsalWeapons  :: Weapon
	,	shipConfSystemUpgrades :: SystemUpgrade
	,	shipConfMaxDamage :: Damage 
	}

data Weapon = Weapon
	{	weaponRange      :: Int
	,	weaponEffect     :: WeaponEffect
	,	weaponReloadTime :: Int
	,	weaponAccuracy   :: Double
	,	weaponFiringCost :: Resources
	}

data SystemUpgrade = 
	  ShieldUpgrade Int
	| HullUpgrade Int
	| EngineUpgrade Int

data WeaponEffect = WeaponEffect
	{	effectShield :: Int
	,	effectHull :: Int
	,	effectPenetration :: Double
	}

----------------- Make Lenses -------------------

makeLenses ''Order
makeLenses ''Goal
makeLenses ''ShipAction
makeLenses ''Weapon
makeLenses ''WeaponEffect
makeLenses ''SystemUpgrade
makeLenses ''ShipType
makeLenses ''ShipConfiguration
makeLenses ''Ship

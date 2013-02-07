{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Ship where

import Serenity.Model.Sector

import Control.Lens
import System.Random

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
	{	_shipTypeFrontWeaponSlots   :: Int
	,	_shipTypeSideWeaponSlots    :: Int
	,	_shipTypeDorsalWeaponSlots  :: Int
	,	_shipTypeSystemUpgradeSlots :: Int
	,	_classMaxDamage :: Damage 
	}

data ShipConfiguration = ShipConfiguration 
	{	_shipConfFrontWeapons   :: [Weapon]
	,	_shipConfSideWeapons    :: [Weapon]
	,	_shipConfDorsalWeapons  :: [Weapon]
	,	_shipConfSystemUpgrades :: SystemUpgrade
	}

data Weapon = Weapon
	{	_weaponRange      :: Int
	,	_weaponEffect     :: WeaponEffect
	,	_weaponReloadTime :: Int
	,	_weaponAccuracy   :: Double
	,	_weaponFiringCost :: Resources
	}

data SystemUpgrade = 
	  ShieldUpgrade Int
	| HullUpgrade Int
	| EngineUpgrade Int

data WeaponEffect = WeaponEffect
	{	_effectShield      :: Int    -- ^ Effect on a shielded ship to shield
	,	_effectHull        :: Int    -- ^ Effect on an unshielded ship to hull
	,	_effectPenetration :: Double -- ^ Probability of damage applying to hull rather than shieled
	}

makeLenses ''Order
makeLenses ''Goal
makeLenses ''ShipAction
makeLenses ''Weapon
makeLenses ''WeaponEffect
makeLenses ''SystemUpgrade
makeLenses ''ShipType
makeLenses ''ShipConfiguration
makeLenses ''Damage
makeLenses ''Ship

applyWeaponDamage :: StdGen -> WeaponEffect -> Ship -> Ship
applyWeaponDamage gen effect ship
	| shielded || penetrated = shipDamage.damageHull   -~ effect^.effectHull   $ ship
	| otherwise              = shipDamage.damageShield -~ effect^.effectShield $ ship
	where
		shielded   = ship^.shipDamage^.damageShield == 0
		penetrated = fst (randomR (0,1) gen) < effect^.effectPenetration

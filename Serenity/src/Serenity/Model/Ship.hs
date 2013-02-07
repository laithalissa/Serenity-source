{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Ship where

import Serenity.Model.Sector
import Serenity.Maths.Util

import Control.Lens
import System.Random

type Location = (Double, Double)
type Direction = (Double, Double)

data Ship = Ship
	{	_shipName :: String
	,	_shipConfiguration :: ShipConfiguration
	,	_shipLocation :: Location
	,	_shipDirection :: Direction
	,	_shipDamage :: Damage
	,	_shipOrder :: Order
	,	_shipGoal :: Goal
	,	_shipPlan :: Plan
	,	_shipBeamTargets :: [Int]
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
	| OrderMove Location (Maybe Direction)
	| OrderAttack Int
	| OrderGuardShip Int
	| OrderGuardPlanet Int
	| OrderGuardLocation Location
	| OrderCapture Int
	deriving (Show, Eq)

data Goal = 
	  GoalNone
	| GoalBeAt Location (Maybe Direction)
	| GoalDestroyed Int
	| GoalGuardShip Int
	| GoalGuardPlanet Int
	| GoalGuardLocation Location
	| GoalCaptured Int
	deriving (Show, Eq)

data ShipAction = 
	ActionMove 
	{	startTime :: Double
	,	startLocDir :: (Location, Direction)
	,	endLocDir   :: (Location, Direction)
	}
	| ActionAttack {targetID :: Int}
	| ActionCapture Int
	|	ActionMoveToEntity Int ShipAction
	deriving (Show, Eq)

type Plan = [ShipAction]

----------------- Weapons and Configuration -------------------

data ShipConfiguration = ShipConfiguration
	{	_shipConfigurationShipClass :: String
	,	_shipConfigurationWeapons :: [Maybe String]
	,	_shipConfigurationSystems :: [Maybe String]
	}
	deriving (Show, Eq)

data Weapon = Weapon
	{	_weaponRange      :: Int
	,	_weaponEffect     :: WeaponEffect
	,	_weaponReloadTime :: Int
	,	_weaponAccuracy   :: Double
	,	_weaponFiringCost :: Resources
	}
	deriving (Show, Eq)

data System = System
	{	_systemShield	:: Int -- ^ additional shield capacity
	,	_systemHull	:: Int -- ^ additional hull capacity
	,	_systemEngine 	:: Int -- ^ additional engine(speed)
	}
	deriving(Show, Eq)

data WeaponType = Side, Special, Turret deriving(Show, Eq)

data WeaponEffect = WeaponEffect
	{	_effectShield      :: Int    -- ^ Effect on a shielded ship to shield
	,	_effectHull        :: Int    -- ^ Effect on an unshielded ship to hull
	,	_effectPenetration :: Double -- ^ Probability of damage applying to hull rather than shieled
	}
	deriving (Show, Eq)

-- ship class --

data ShipClass = ShipClass
	{	shipClassName 			:: String
	,	shipClassCenterOfRotation	:: Location
	,	shipClassWeaponSlots		:: [WeaponSlot]
	,	shipClassSystemSlots		:: [SystemSlot]
	}
	deriving (Show, Eq)

data WeaponSlot = WeaponSlot
	{	weaponSlotName 		:: String
	,	weaponSlotLocation	:: Location
	,	weaponSlotDirection	:: Direction
	,	weaponSlotType 		:: WeaponType
	}
	deriving (Show, Eq)

data SystemSlot = SystemSlot
	{	systemSlotName 		:: String
	,	systemSlotLocation 	:: Location
	,	systemSlotDirection	:: Direction
	}
	deriving (Show, Eq)


makeLenses ''Order
makeLenses ''Goal
makeLenses ''ShipAction
makeLenses ''Weapon
makeLenses ''WeaponEffect
makeLenses ''System
makeLenses ''Damage
makeLenses ''Ship

applyWeaponDamage :: StdGen -> WeaponEffect -> Ship -> Ship
applyWeaponDamage gen effect ship
	| shielded || penetrated = shipDamage.damageHull   %~ updateHull $ ship
	| otherwise              = shipDamage.damageShield %~ updateShield $ ship
	where
		updateHull   = f (ship^.shipType.shipTypeMaxDamage.damageHull) (effect^.effectHull)
		updateShield = f (ship^.shipType.shipTypeMaxDamage.damageShield) (effect^.effectShield)
		f maxDamage a b = rangeLimitAttainBounds 0 maxDamage (b+a)
		shielded   = ship^.shipDamage^.damageShield == 0
		penetrated = fst (randomR (0,1) gen) < effect^.effectPenetration

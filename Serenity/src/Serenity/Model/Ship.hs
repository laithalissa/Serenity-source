{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Ship where

import Serenity.Model.Sector

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M (empty)

type Location = (Double, Double)
type Direction = (Double, Double)

data Ship = Ship
	{	_shipConfiguration :: ShipConfiguration
	,	_shipLocation :: Location
	,	_shipDirection :: Direction
	,	_shipDamage :: Damage
	,	_shipOrder :: Order
	,	_shipGoal :: Goal
	,	_shipPlan :: Plan
	,	_shipTargets :: Map Int [Int] -- ^ Map from weapon ID to list of targets
	}
	deriving (Show, Eq)

initShip :: ShipConfiguration -> Location -> Direction -> Ship
initShip conf location direction = Ship
	{	_shipConfiguration=conf
	,	_shipLocation=location
	,	_shipDirection=direction
	,	_shipDamage=Damage 0 0
	,	_shipOrder=OrderNone
	,	_shipGoal=GoalNone
	,	_shipPlan=[]
	,	_shipTargets = M.empty
	}

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
	| ActionCapture {targetID :: Int}
	| ActionMoveToEntity Int ShipAction
	-- | ActionOrbit {targetID :: Int}
	deriving (Show, Eq)

type Plan = [ShipAction]

----------------- Weapons and Configuration -------------------

data ShipConfiguration = ShipConfiguration
	{	_shipConfigurationShipClass :: String
	,	_shipConfigurationWeapons   :: [Maybe String]
	,	_shipConfigurationSystems   :: [Maybe String]
	}
	deriving (Show, Eq)

dreadnoughtConfiguration = ShipConfiguration
	{	_shipConfigurationShipClass = "Dreadnought"
	,	_shipConfigurationWeapons = replicate 9 (Just "Laser")
	,	_shipConfigurationSystems = []
	}

destroyerConfiguration = ShipConfiguration
	{	_shipConfigurationShipClass = "Destroyer"
	,	_shipConfigurationWeapons = replicate 5 (Just "Laser")
	,	_shipConfigurationSystems = []
	}

corvetteConfiguration = ShipConfiguration
	{	_shipConfigurationShipClass = "Corvette"
	,	_shipConfigurationWeapons = [Just "Laser"]
	,	_shipConfigurationSystems = []
	}

data Weapon = Weapon
	{	_weaponRange      :: Int
	,	_weaponEffect     :: WeaponEffect
	,	_weaponReloadTime :: Int
	,	_weaponAccuracy   :: Double
	,	_weaponFiringCost :: Resources
	}
	deriving (Show, Eq)

data System = System
	{	_systemShield :: Int -- ^ additional shield capacity
	,	_systemHull   :: Int -- ^ additional hull capacity
	,	_systemEngine :: Int -- ^ additional engine(speed)
	}
	deriving(Show, Eq)

data WeaponEffect = WeaponEffect
	{	_effectShield      :: Int    -- ^ Effect on a shielded ship to shield
	,	_effectHull        :: Int    -- ^ Effect on an unshielded ship to hull
	,	_effectPenetration :: Double -- ^ Probability of damage applying to hull rather than shieled
	}
	deriving (Show, Eq)


-- ship class --

data ShipClass = ShipClass
	{	_shipClassCenterOfRotation :: Location
	,	_shipClassSpeed            :: Double
	,	_shipClassMaxDamage        :: Damage
	,	_shipClassWeaponSlots      :: [WeaponSlot]
	,	_shipClassSystemSlots      :: [SystemSlot]
	,	_shipClassAssetName        :: String
	}
	deriving (Show, Eq)

data SlotType =
		Front
	|	Side
	|	Turret
	deriving (Read, Show, Eq)

data WeaponSlot = WeaponSlot
	{	_weaponSlotLocation  :: Location
	,	_weaponSlotDirection :: Direction
	,	_weaponSlotType      :: SlotType
	}
	deriving (Show, Eq)

data SystemSlot = SystemSlot
	{	_systemSlotLocation  :: Location
	,	_systemSlotDirection :: Direction
	}
	deriving (Show, Eq)


makeLenses ''Ship
makeLenses ''Damage
makeLenses ''Order
makeLenses ''Goal
makeLenses ''ShipAction
makeLenses ''ShipConfiguration
makeLenses ''Weapon
makeLenses ''System
makeLenses ''WeaponEffect
makeLenses ''ShipClass
makeLenses ''WeaponSlot
makeLenses ''SystemSlot

---------- Lens Helpers ----------




-- applyWeaponDamage :: StdGen -> WeaponEffect -> Ship -> Ship
-- applyWeaponDamage gen effect ship
-- 	| shielded || penetrated = shipDamage.damageHull   %~ updateHull $ ship
-- 	| otherwise              = shipDamage.damageShield %~ updateShield $ ship
-- 	where
-- 		updateHull   = f (ship^.shipType.shipTypeMaxDamage.damageHull) (effect^.effectHull)
-- 		updateShield = f (ship^.shipType.shipTypeMaxDamage.damageShield) (effect^.effectShield)
-- 		f maxDamage a b = rangeLimitAttainBounds 0 maxDamage (b+a)
-- 		shielded   = ship^.shipDamage^.damageShield == 0
-- 		penetrated = fst (randomR (0,1) gen) < effect^.effectPenetration

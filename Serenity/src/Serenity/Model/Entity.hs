{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Entity where

import Control.Lens

type EntityID = Int
type OwnerID = Int

data Entity a = 
	Entity
	{	_entityID :: EntityID
	,	_ownerID :: OwnerID
	,	_entityData :: a
	} deriving(Show)

instance Eq (Entity a) where
	a == b = (_entityID a) == (_entityID b)

instance Ord (Entity a) where
	compare a b = compare (_entityID a) (_entityID b)

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

data Torpedo = Torpedo
	{	_torpedoLocation :: (Double, Double)
	,	_torpedoDirection :: (Double, Double)
	,	_torpedoSpeed :: Float
	}
	deriving (Show, Eq)

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

makeLenses ''Entity
makeLenses ''Order
makeLenses ''Goal
makeLenses ''ShipAction
makeLenses ''Torpedo
makeLenses ''Ship

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
	}
	deriving (Show, Eq)

data Damage = Damage 
	{	_damageHull   :: Int
	,	_damageSheild :: Int
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
	| OrderMove (Double, Double) 
	| OrderAttack Int
	| OrderGuard Int
	deriving (Show, Eq)

data Plan = 
	  PlanNone
	| PlanMove (Double, Double) 
	| PlanAttack Int
	deriving (Show, Eq)

makeLenses ''Entity
makeLenses ''Plan
makeLenses ''Order
makeLenses ''Torpedo
makeLenses ''Ship

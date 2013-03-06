{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Entity
(	Entity(..)
,	EntityID
,	OwnerID
,	Torpedo(..)
,	entityID
,	ownerID
,	entityData
,	torpedoLocation
,	torpedoDirection
,	torpedoSpeed
,	module Serenity.Model.Ship
) where

import Serenity.Model.Common
import Serenity.Model.Ship

import Control.Lens

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

data Torpedo = Torpedo
	{	_torpedoLocation :: (Double, Double)
	,	_torpedoDirection :: (Double, Double)
	,	_torpedoSpeed :: Float
	}
	deriving (Show, Eq)

makeLenses ''Torpedo
makeLenses ''Entity

{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Serenity.Model.Game where

import Serenity.Model.Entity
import Serenity.Model.Sector

import Data.Vinyl
import Data.Map (Map)
import qualified Data.Map as Map

-- | The game state.
type Game = Rec ["sector" ::: Sector, "ships" ::: Map EntityID Ship]
_sector      = Field :: "sector" ::: Sector
_entities    = Field :: "ships"  ::: Map EntityID Ship

_updateLocation = Field :: "updateLocation" ::: (Float, Float)
_order = Field :: "order" ::: Order

defaultGame = 
	    _sector   =: sectorOne
	<+> _entities =: Map.fromList []


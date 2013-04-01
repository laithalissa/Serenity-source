{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Fleet where

import Control.Lens

import Serenity.Model.Ship

initFleet :: [ShipConfiguration] -> Fleet
initFleet = Fleet 

demoFleet :: Fleet
demoFleet = Fleet $ replicate 8 demoShipConfiguration
	
data Fleet = Fleet
	{	_fleetShips :: [ShipConfiguration]
	}
	deriving(Show, Eq)
makeLenses ''Fleet



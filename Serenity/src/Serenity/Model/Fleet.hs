{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Fleet where

import Control.Lens

import Serenity.Model.Ship
import Data.Binary
import Data.DeriveTH

initFleet :: [ShipConfiguration] -> Fleet
initFleet confs = Fleet 
	{	_fleetName = "Untitled Fleet"
	,	_fleetShips = confs
	}

demoFleet :: Fleet
demoFleet = Fleet 
	{	_fleetName = "Demo Fleet"
	,	_fleetShips = 
		[	dreadnoughtConfiguration "The Relentless"
		,	destroyerConfiguration   "Orgu"
		,	destroyerConfiguration   "Oenone"
		,	destroyerConfiguration   "Beezling"
		,	destroyerConfiguration   "The Gluian"
		,	corvetteConfiguration    "The Falcon"
		,	corvetteConfiguration    "The Eagle"
		,	corvetteConfiguration    "The Osprey"
		,	corvetteConfiguration    "The Peregrine"
		,	corvetteConfiguration    "The Hawk"
		,	corvetteConfiguration    "The Kestrel"
		]
	}
	
data Fleet = Fleet
	{	_fleetName :: String
	,	_fleetShips :: [ShipConfiguration]
	}
	deriving(Show, Eq)
makeLenses ''Fleet

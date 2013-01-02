{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Sector where

import Data.Vinyl
import Data.AdditiveGroup
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Binary
import Data.DeriveTH

_name         = Field :: "name"         ::: String
_size         = Field :: "size"         ::: (Double, Double)
_spawnPoints  = Field :: "spawnPoints"  ::: [(Double, Double)]
_planets      = Field :: "planets"      ::: Map PlanetID Planet
_spaceLanes   = Field :: "spaceLanes"   ::: [SpaceLane]

type SpaceLane = (PlanetID, PlanetID)

type Sector = Rec 
	[	"name"         ::: String
	,	"size"         ::: (Double, Double)
	,	"spawnPoints"  ::: [(Double, Double)]
	,	"planets"      ::: Map PlanetID Planet
	,	"spaceLanes"   ::: [SpaceLane]	
	]

type PlanetID = Int

_pID       = Field :: "pID"       ::: PlanetID
_ecotype   = Field :: "ecotype"   ::: Ecotype
_location  = Field :: "location"  ::: (Double, Double)
_resources = Field :: "resources" ::: Resources

type Planet = Rec 
	[	"pID"       ::: PlanetID
	,	"name"      ::: String
	,	"ecotype"   ::: Ecotype
	,	"location"  ::: (Double, Double)
	,	"resources" ::: Resources
	]

data Ecotype = 
	  Blue
	| Dessert
	| Metal
	| Ocean
	deriving (Show, Eq)

_fuel       = Field :: "fuel"       ::: Int
_metal      = Field :: "metal"      ::: Int
_antimatter = Field :: "antimatter" ::: Int

type Resources = Rec 
	[	"fuel"       ::: Int
	,	"metal"      ::: Int
	,	"antimatter" ::: Int
	]

res a b c = _fuel =: a <+> _metal =: b <+> _antimatter =: c

instance AdditiveGroup Resources where
	zeroV = _fuel =: 0 <+> _metal =: 0 <+> _antimatter =: 0

	a ^+^ b = 
		    (_fuel       =: (+) (a^.(rLens _fuel))        (b^.(rLens _fuel)))
		<+> (_metal      =: (+) (a^.(rLens _metal))       (b^.(rLens _metal)))
		<+> (_antimatter =: (+) (a^.(rLens _antimatter))  (b^.(rLens _antimatter))) where

	negateV a = 
		    (_fuel       =: (- a^.(rLens _fuel)))
		<+> (_metal      =: (- a^.(rLens _metal)))
		<+> (_antimatter =: (- a^.(rLens _antimatter)))

sectorOne = 
	    _name        =: "Sector One"
	<+> _size        =: (200, 200)
	<+> _spawnPoints =: [(10,10), (10,190), (190,190), (190,10)]
	<+> _planets     =: Map.fromList
		[	(1, _pID =: 1 <+> _name =: "Splearth"  <+> _ecotype =: Blue    <+> _location  =: (10 , 100) <+> _resources =: res 10 10 0)
		,	(2, _pID =: 2 <+> _name =: "Tatooine"  <+> _ecotype =: Dessert <+> _location  =: (100, 100) <+> _resources =: res 10 10 0)
		,	(3, _pID =: 3 <+> _name =: "Qoruscant" <+> _ecotype =: Metal   <+> _location  =: (190, 190) <+> _resources =: res 10 0 10)
		]
	<+> _spaceLanes  =: [(1,2), (2,3)]


$(derive makeBinary ''Ecotype)
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Serenity.Model.Entity where

import Data.Vinyl

type EntityID = Int

_eID          = Field :: "eID"           ::: EntityID
_name         = Field :: "name"          ::: String
_location     = Field :: "location"      ::: (Double, Double)
_direction    = Field :: "direction"     ::: (Double, Double)

_hullDamage   = Field :: "hullDamage"    ::: Int
_sheildDamage = Field :: "sheildDamage"  ::: Int

_order = Field :: "order" ::: Order
data  Order = 
	  OrderNone
	| OrderMove (Double, Double) 
	| OrderAttack Int
	| OrderGuard Int
	deriving (Show, Eq)

_plan = Field :: "plan" ::: Plan
data Plan = 
	  PlanNone
	| PlanMove (Double, Double) 
	| PlanAttack Int
	deriving (Show, Eq)

type Damage = Rec 
	[	"hullDamage"   ::: Int
	,	"sheildDamage" ::: Int
	]

type Ship = Rec
	[	"eID"       ::: Int
	,	"name"      ::: String
	,	"location"  ::: (Double, Double)
	,	"direction" ::: (Double, Double)
	,	"damage"    ::: Damage
	,	"order"     ::: Order
	]
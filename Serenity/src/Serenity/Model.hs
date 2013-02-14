

module Serenity.Model
(	module Serenity.Model.Entity
,	module Serenity.Model.Fleet
,	module Serenity.Model.Game
,	module Serenity.Model.Sector
,	module Serenity.Model.Time
,	module Serenity.Model.Message
) where

import Serenity.Model.Entity
import Serenity.Model.Fleet
import Serenity.Model.Game
import Serenity.Model.Sector
import Serenity.Model.Time
import Serenity.Model.Message


---------- Lens Helpers ----------

gameMap' = gameBuilder.gbSector

shipClass' :: Entity Ship -> Game -> ShipClass
shipClass' entity game = 
	let
		shipClassName = entity^.entityData.shipConfiguration.shipConfigurationShipClass
	in
		fromJust $ Map.lookup shipClassName (game^.gameBuilder.gbShipClasses)

shipMaxHealth' :: Entity Ship -> Game -> Int
shipMaxHealth' entity game = (shipClass' entity game)^.shipClassMaxDamage.damageHull

shipCurrentDamage' :: Entity Ship -> Int
shipCurrentDamage' entity = entity^.entityData.shipDamage.damageHull

shipHealth' :: Entity Ship -> Game -> Int
shipHealth' entity game = (shipMaxHealth' entity game) - (shipCurrentDamage' entity)

gameEntity' :: EntityID -> Game -> Entity Ship
gameEntity' eID game = fromJust $ Map.lookup eID (game^.gameShips)
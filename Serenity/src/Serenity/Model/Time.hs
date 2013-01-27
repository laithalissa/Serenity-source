{-# LANGUAGE Arrows, TypeFamilies #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}

module Serenity.Model.Time 
(	Command
,	Update
,	Updateable(..)
,	Commandable(..)
,	Evolvable(..)
) where

import Prelude hiding (id, (.))

import Serenity.Model.Game
import Serenity.Model.Entity
import Serenity.Model.Message
import Serenity.Model.Wire
import Serenity.Maths.Util

import Control.Lens
import Data.Map (Map, elems, adjust)
import Data.Maybe (catMaybes)

class Updateable a where
	update  ::  Update  -> a -> a
	updates :: [Update] -> a -> a
	updates = flip (foldr update)

class (Updateable a) => Evolvable a where
	evolve :: Game -> UpdateWire a
	evolve _ = pure []

class (Updateable a) => Commandable a where
	command  ::  Command  -> a -> [Update] 
	commands :: [Command] -> a -> [Update]
	command _ _ = []
	commands cs a = concatMap (flip command a) cs

instance Updateable Game where
	update UpdateEntity {updateEntity=entity} game = gameShips.(at i) .~ Just entity $ game where i=entity^.entityID
	update AddEntity    {updateEntity=entity} game = gameShips.(at i) .~ Just entity $ game where i=entity^.entityID
	update DeleteEntity {updateEntity=entity} game = gameShips.(at i) .~ Nothing $ game where i=entity^.entityID

	update UpdateEntityLocation{updateEntityID=eID, updateEntityLocation=loc} game = 
		gameShips.(at eID).traverse.entityData.shipLocation .~ (pFloat2Double loc) $ game

	update UpdateEntityDirection{updateEntityID=eID, updateEntityDirection=dir} game = 
		gameShips.(at eID).traverse.entityData.shipDirection .~ (pFloat2Double dir) $ game

	update UpdateEntityLocationDirection{updateEntityID=eID, updateEntityLocation=loc, updateEntityDirection=dir} game = 
		updates [UpdateEntityLocation eID loc, UpdateEntityDirection eID dir] game

	update UpdateShipOrder{updateEntityID=eID, updateShipOrder=order} game = 
		gameShips.(at eID).traverse.entityData.shipOrder .~ order $ game

	update _ g = g

instance Evolvable Game where
	evolve game = (arr concat) . (mapEvolve game) . (pure $ elems $ game^.gameShips) where
		mapEvolve game = proc ents -> do
			case ents of 
				(e:es) -> do
					u  <-    evolve game -< e
					us <- mapEvolve game -< es
					id -< u:us
				[] -> id -< []

instance Commandable Game where
	command c@GiveOrder{commandEntityID = cID, order=order} game = concatMap (command c) (catMaybes [game^.gameShips.(at cID)])
	command _ _ = []

instance (Updateable a) => Updateable (Entity a) where
	update u Entity{_entityData=d} = Entity{_entityData= update u d}
instance (Evolvable a) => Evolvable (Entity a) where
	evolve game = proc Entity{_entityData=d} -> do
		evolve game -< d
instance (Commandable a) => Commandable (Entity a) where
	command c Entity{_entityData=d} = command c d

instance Updateable Ship where
	update _ s = s
instance Commandable Ship where
	command GiveOrder{commandEntityID=cID, order=order} ship = return UpdateShipOrder{updateEntityID=cID, updateShipOrder=order}
	command _ _ = []

instance Evolvable Ship where
	evolve _ = pure []


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

import Control.Lens
import Serenity.Model.Wire
import Data.Map (Map, elems, adjust)

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
	update UpdateEntity {updateEntity=entity} game = undefined -- over gameShips (adjust (\_ -> entity) (entity^.entityID)) game
	update AddEntity    {updateEntity=entity} game = undefined
	update DeleteEntity {updateEntity=entity} game = undefined
	update UpdateEntityLocation{updateEntityID=eID, updateEntityLocation=loc} game = undefined
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
	command c game = [] --elems $ game ^. (rLens _entities)

instance Updateable Ship where
	update _ s = s

instance Evolvable Ship where
	evolve _ = pure []

instance Updateable (Entity a) where
	update _ s = s

instance Evolvable (Entity a) where
	evolve _ = pure []



{-# LANGUAGE Arrows, TypeFamilies #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}

module Serenity.Model.Time 
(	Command
,	Update
,	defaultGame
,	Updateable(..)
,	Commandable(..)
,	Evolvable(..)
) where

import Prelude hiding (id, (.))

import Serenity.Model.Game
import Serenity.Model.Entity
import Serenity.Network.Message

import Serenity.Extensions.Vinyl
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
	update UpdateEntity {updateEntity=Ship_ entity} game = rMod _entities (adjust (\_ -> entity) (entity^.(rLens _eID))) game
	update AddEntity    {updateEntity=Ship_ entity} game = undefined
	update DeleteEntity {updateEntity=Ship_ entity} game = undefined
	update UpdateEntityLocation{updateEntityID=eID, updateEntityLocation=loc} game = undefined
	update _ g = g

instance Evolvable Game where
	evolve game = (arr concat) . (mapEvolve game) . (pure $ elems $ _entities `rGet` game) where
		mapEvolve game = proc entities -> do
			case entities of 
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




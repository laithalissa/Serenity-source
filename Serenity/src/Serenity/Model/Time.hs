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
,	filteredUpdates
) where

import Debug.Trace(trace)

import Serenity.AI.Plan
import Serenity.Maths.Util
import Serenity.Model.Game
import Serenity.Model.Entity
import Serenity.Model.Message
import Serenity.Model.Wire

import Control.Lens
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.VectorSpace
import Prelude hiding (id, (.))

class Updateable a where
	update  ::  Update  -> a -> a
	updates :: [Update] -> a -> a
	updates = flip (foldr update)

class (Updateable a) => Evolvable a where
	evolve :: UpdateWire (a, Game)
	evolve = pure []

class (Updateable a) => Commandable a where
	command  ::  Command  -> a -> [Update] 
	commands :: [Command] -> a -> [Update]
	command _ _ = []
	commands cs a = concatMap (flip command a) cs

instance Updateable Game where
	update UpdateEntity {updateEntity=entity} game = gameShips.(at i) .~ Just entity $ game where i=entity^.entityID
	update AddEntity    {updateEntity=entity} game = gameShips.(at i) .~ Just entity $ game where i=entity^.entityID
	update DeleteEntity {updateEntityID=eID} game = gameShips.(at eID) .~ Nothing $ game

	update UpdateEntityLocation{updateEntityID=eID, updateEntityLocation=loc} game = 
		gameShips.(at eID).traverse.entityData.shipLocation .~ (pFloat2Double loc) $ game

	update UpdateEntityDirection{updateEntityID=eID, updateEntityDirection=dir} game = 
		gameShips.(at eID).traverse.entityData.shipDirection .~ (pFloat2Double dir) $ game

	update UpdateEntityLocationDirection{updateEntityID=eID, updateEntityLocation=loc, updateEntityDirection=dir} game = 
		updates [UpdateEntityLocation eID loc, UpdateEntityDirection eID dir] game

	update UpdateShipOrder{updateEntityID=eID, updateShipOrder=order} game = 
		gameShips.(at eID).traverse.entityData.shipOrder .~ order $ 
		gameShips.(at eID).traverse.entityData.shipPlan .~ [] $ 
		gameShips.(at eID).traverse.entityData.shipActionStartTime .~ 0.0 $ 
		gameShips.(at eID).traverse.entityData.shipGoal .~ GoalNone $ game
	

	update UpdateShipPlan{updateEntityID=eID, updateShipPlan=plan} game = 
		gameShips.(at eID).traverse.entityData.shipPlan .~ plan $ 
		gameShips.(at eID).traverse.entityData.shipActionStartTime .~ 0.0 $ game

	update UpdateShipGoal{updateEntityID=eID, updateShipGoal=goal} game = 
		gameShips.(at eID).traverse.entityData.shipGoal .~ goal $ 
		gameShips.(at eID).traverse.entityData.shipPlan .~ [] $ 
		gameShips.(at eID).traverse.entityData.shipActionStartTime .~ 0.0 $ game

	update UpdateShipDamage{updateEntityID=eID, updateShipDamage=damage} game =
		gameShips.(at eID).traverse.entityData.shipDamage .~ damage $ game

	update UpdateShipBeamTargets{updateEntityID=eID, updateShipBeamTargets=targets} game =
		gameShips.(at eID).traverse.entityData.shipBeamTargets .~ targets $ game

	update UpdateGameRanks{updateGameRanks=ranks} game = gameRanks .~ ranks $ game

	update UpdateGameOver game = game

instance Evolvable Game where
	evolve = proc (game, _) -> do
		x <- mapEvolve -< (M.elems $ game^.gameShips, game)
		arr concat -< x ++ [checkGameEnd game]

mapEvolve = proc (ents, game) -> do
	case ents of
		(e:es) -> do
			u  <-    evolve -< (e, game)
			us <- mapEvolve -< (es, game)
			id -< u:us
		[] -> id -< []

instance Commandable Game where
	command c@GiveOrder{commandEntityID = cID} game = concatMap (command c) (catMaybes [game^.gameShips.(at cID)])

instance Updateable (Entity Ship) where
	update _ = id

instance Commandable (Entity Ship) where
	command GiveOrder{commandEntityID=cID, order=order} _ = return UpdateShipOrder{updateEntityID=cID, updateShipOrder=order}

instance Evolvable (Entity Ship) where
	evolve = proc (entity@Entity{_entityData=ship}, game) -> do
		upT <- evolveShipTargets -< (entity, game)
		upP <- evolveShipPlan -< (entity, game)
		upD <- evolveShipDamage -< (entity, game)
		id -< (upT ++ upP ++ upD)



filteredUpdates :: [Update] -> [Update]
filteredUpdates updates' = resultUpdates
	where
	resultUpdates = filter (f updates') updates'
	f :: [Update] -> Update -> Bool
	f updates (UpdateEntity entity) = not $ hasDelete' entity updates
	f updates (AddEntity entity) = not $ hasDelete' entity updates
	f updates (DeleteEntity eID) = True
	f updates update = not $ hasDelete (updateEntityID update) updates

	hasDelete' :: Entity Ship -> [Update] -> Bool
	hasDelete' entity updates = hasDelete (entity^.entityID) updates
	hasDelete :: EntityID -> [Update] -> Bool
	hasDelete eID [] = False
	hasDelete eID (u:us) = case u of
		(DeleteEntity eID') -> if (eID == eID') then True else hasDelete eID us
		_ -> hasDelete eID us



evolveShipDamage :: UpdateWire (Entity Ship, Game)
evolveShipDamage = proc (entity, game) -> do
	case (health entity game) of
		0 -> id -< [DeleteEntity (entity^.entityID)]
		h -> id -< damageTargets entity game
		where
		lostHealth entity = entity^.entityData.shipDamage.damageHull
		totalHealth entity game = (fromJust $ M.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) (game^.gameBuilder^.gbShipClasses))^.shipClassMaxDamage^.damageHull
		health entity game = (totalHealth entity game) - (lostHealth entity)
		damageTargets entity game = concatMap (damageTarget game) (entity^.entityData.shipBeamTargets)
		damageTarget game target = case M.lookup target (game^.gameShips) of
			Just entity -> [UpdateShipDamage (entity^.entityID) (entity^.entityData.shipDamage & damageHull +~ 1)]
			Nothing -> []

evolveShipTargets :: UpdateWire (Entity Ship, Game)
evolveShipTargets = proc (entity@Entity{_entityData=ship}, game) -> do
	let targets = M.keys $ M.filter (otherInRange entity) (game^.gameShips)
	if (not $ null targets) || (not $ null $ ship^.shipBeamTargets)
		then id -< [UpdateShipBeamTargets (entity^.entityID) targets]
		else id -< []
	where
		otherInRange e t = e /= t && (e^.ownerID) /= (t^.ownerID) && inRange (e^.entityData) t

-- | Check if the target ship is in range
inRange
	:: Ship -- ^ Ship
	-> Entity Ship -- ^ Target
	-> Bool
inRange ship target = magnitude ((ship^.shipLocation) - (target^.entityData.shipLocation)) < 25

checkGameEnd :: Game -> [Update]
checkGameEnd game = case game^.gameGameMode of
	DeathMatch ->
		if all (== head playersLeft) (tail playersLeft)
			then [UpdateGameRanks $ (head playersLeft, 1):(updateRanks), UpdateGameOver]
			else if not $ null deadPlayers
				then [UpdateGameRanks updateRanks]
				else []
	_ -> []
	where
		playersLeft = map _ownerID (M.elems $ game^.gameShips)
		deadPlayers = filter (\p -> p `notElem` playersLeft && p `notElem` (map fst $ game^.gameRanks)) (game^.gamePlayers)
		updateRanks = (game^.gameRanks) ++ (map (\p -> (p, (length (game^.gamePlayers)) - (length (game^.gameRanks)))) deadPlayers)

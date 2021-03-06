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

import Serenity.AI.Plan
import Serenity.AI.Targeting
import Serenity.Maths.Util
import Serenity.Model.Game
import Serenity.Model.Entity
import Serenity.Model.Message
import Serenity.Model.Sector
import Serenity.Model.Wire

import Control.Lens
import Data.List (nub)
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
		gameShips.(at eID).traverse.entityData.shipGoal .~ GoalNone $ game

	update UpdateShipPlan{updateEntityID=eID, updateShipPlan=plan} game = 
		gameShips.(at eID).traverse.entityData.shipPlan .~ plan $ game

	update UpdateShipGoal{updateEntityID=eID, updateShipGoal=goal} game = 
		gameShips.(at eID).traverse.entityData.shipGoal .~ goal $ game

	update UpdateShipDamage{updateEntityID=eID, updateShipDamage=damage} game =
		gameShips.(at eID).traverse.entityData.shipDamage .~ damage $ game

	update UpdateShipTargets{updateEntityID=eID, updateShipTargets=targets} game =
		gameShips.(at eID).traverse.entityData.shipTargets .~ targets $ game

	update UpdatePlanetCapture{updatePlanetID=pID, updateCaptured=captured} game =
		gamePlanets.(at pID).traverse.planetCaptured .~ captured $ game

	update UpdateGameRanks{updateGameRanks=ranks} game = gameRanks .~ ranks $ game

	update UpdateGameOver game = game

instance Evolvable Game where
	evolve = proc (game, _) -> do
		x <- mapEvolve -< (M.elems $ game^.gameShips, game)
		y <- mapEvolve -< (M.elems $ game^.gamePlanets, game)
		arr concat -< x ++ y ++ [checkGameEnd game]

mapEvolve = proc (evolvables, game) -> do
	case evolvables of
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

instance Updateable Planet where
	update _ = id

instance Evolvable Planet where
	evolve = proc (planet@Planet{_planetLocation=loc}, game) -> do
		let ships = filter (capturingPlanet loc . (^.entityData.shipLocation)) (M.elems $ game^.gameShips)
		let players = nub $ map _ownerID ships
		if length players == 1
			then id -< updatePlanetCaptured (_planetID planet) (_planetCaptured planet) (head players)
			else id -< decayPlanetCapture (_planetID planet) (_planetCaptured planet)

capturingPlanet loc ship = magnitude (ship - loc) <= 25

updatePlanetCaptured :: Int -> (Int, Int) -> Int -> [Update]
updatePlanetCaptured planet (player, percent) player'
	| player == player' && percent < 100 = [UpdatePlanetCapture planet (player, percent + 2)]
	| player /= player' && percent > 0 = [UpdatePlanetCapture planet (player, percent - 2)]
	| player /= player' = [UpdatePlanetCapture planet (player', 0)]
	| otherwise = []

decayPlanetCapture :: Int -> (Int, Int) -> [Update]
decayPlanetCapture planet (player, percent)
	| percent <= 0 && player /= -1 = [UpdatePlanetCapture planet (-1, 0)]
	| percent < 100 && player /= -1 = [UpdatePlanetCapture planet (player, percent - 1)]
	| otherwise = []

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
		damageTargets entity game = concatMap (damageTarget game) (concat . M.elems $ entity^.entityData.shipTargets)
		damageTarget game target = case M.lookup target (game^.gameShips) of
			Just entity -> [UpdateShipDamage (entity^.entityID) (entity^.entityData.shipDamage & damageHull +~ 1)]
			Nothing -> []

evolveShipTargets :: UpdateWire (Entity Ship, Game)
evolveShipTargets = proc (entity@Entity{_entityData=ship}, game) -> do
	let targets = findShipTargets entity game
	if targets /= (ship^.shipTargets)
		then id -< [UpdateShipTargets (entity^.entityID) targets]
		else id -< []

checkGameEnd :: Game -> [Update]
checkGameEnd game = case game^.gameGameMode of
	DeathMatch -> case length playersLeft of
		0 -> [UpdateGameRanks updateRanks, UpdateGameOver]
		1 -> [UpdateGameRanks $ (head playersLeft, 1):updateRanks, UpdateGameOver]
		_ -> if not $ null deadPlayers
			then [UpdateGameRanks updateRanks]
			else []
	_ -> []
	where
		playersLeft = nub $ map _ownerID (M.elems $ game^.gameShips)
		deadPlayers = filter (\p -> p `notElem` playersLeft && p `notElem` (map fst $ game^.gameRanks)) (map fst $ game^.gamePlayers)
		updateRanks = (map (\p -> (p, (length playersLeft) + 1)) deadPlayers) ++ (game^.gameRanks)

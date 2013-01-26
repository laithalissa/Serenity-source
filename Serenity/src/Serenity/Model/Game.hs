{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Game
(	Game
,	gameSector
,	gameShips
,	defaultGame
) where

import Serenity.Model.Entity
import Serenity.Model.Sector

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

data Game = Game
	{	_gameSector :: Sector
	,	_gameShips  :: Map EntityID (Entity Ship)
	}
makeLenses ''Game

defaultGame = Game
	{	_gameSector = sectorOne
	,	_gameShips  = Map.fromList []
	}

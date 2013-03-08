{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.End where

import Serenity.External
import Serenity.Game.Client.ClientState
import Serenity.Game.UI.Application
import Serenity.Model.Game
import Serenity.Sheen

import Control.Lens
import Control.Monad.State
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)

data EndData a = EndData
	{	_endRankingsTable :: Table a
	,	_endContinueButton :: Button a a
	}
makeLenses ''EndData

class AppState a => EndState a where
	aEnd :: Simple Lens a (EndData a)
	aClientState :: Simple Lens a (Maybe ClientState)

initEndData :: EndState a => Assets -> EndData a
initEndData assets = EndData
	{	_endRankingsTable = initTable 50 Nothing
	,	_endContinueButton = initMenuButton "Continue" continueAction
	}

continueAction :: EndState a => a -> a
continueAction = execState $ do
	aClientState .= Nothing
	aMode .= Menu

viewEnd :: EndState a => a -> View a
viewEnd a = (initView ((0, 0), (1024, 750)))
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	(initBox ((25, 625), (630, 75))) <++ -- Winner box
		if isJust $ a^.aClientState
			then [labelStatic a (StaticString $ "Winner: " ++ (show (game^.gameRanks.to head._1))) (bright green) Nothing 4 ((10,10), (40,45))]
			else []
	,	(initBox ((25, 50), (630, 550))) <++ -- Rankings table
		if isJust $ a^.aClientState
			then [table a (aEnd.endRankingsTable) (to rankings) rankLabel ((25, 25), (580, 500))]
			else []
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aEnd.endContinueButton) id ((80,50),(185,28))
		]
	]
	where
		game = a^.aClientState.(to fromJust).clientGame
		rankings _ = sortBy rankCompare (game^.gameRanks)
		rankCompare (_, r) (_, r') = case game^.gameGameMode of
			_ -> compare r r'
		rankLabel (player, rank) = case game^.gameGameMode of
			_ -> labelStatic a (StaticString $ (show rank) ++ ". " ++ (show player)) (bright green) Nothing 2 ((0,0), (580,25))

timeEnd :: EndState a => Float -> a -> a
timeEnd _ = id

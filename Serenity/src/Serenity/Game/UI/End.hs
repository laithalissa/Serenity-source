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
import Data.Maybe (fromJust, fromMaybe, isJust)

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
	{	_endRankingsTable = initTable 1 50 Nothing
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
			then [labelStatic a ((initLabel (StaticString $ "Winner: " ++ winner) (bright green) Nothing) & (labelScale .~ 4)) ((10,10), (40,45))]
			else []
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aEnd.endContinueButton) id ((80,50),(185,28))
		]
	] 
	++ if isJust $ a^.aClientState -- Ranking table
		then [table a (aEnd.endRankingsTable) (to rankings) rankLabel ((25, 25), (580, 560))]
		else []
	where
		game = a^.aClientState.(to fromJust).clientGame
		winner = fromMaybe "" $ (flip lookup) (game^.gamePlayers) $ fst $ head $ sortBy rankCompare (game^.gameRanks)
		rankings _ = sortBy rankCompare (game^.gameRanks)
		rankCompare (_, r) (_, r') = compare r r'
		rankLabel (player, rank) = (initBox ((0, 50), (630, 40))) <++ [labelStatic a lab ((5,5), (580,25))] where
			lab = (initLabel (StaticString $ (show rank) ++ ". " ++ (fromMaybe "" $ lookup player (game^.gamePlayers))) (bright green) Nothing)
				& (labelScale .~ 2)

timeEnd :: EndState a => Float -> a -> a
timeEnd _ = id

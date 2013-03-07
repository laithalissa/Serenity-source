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
	{	_endWinnerLabel :: Label a
	,	_endContinueButton :: Button a a
	}
makeLenses ''EndData

class AppState a => EndState a where
	aEnd :: Simple Lens a (EndData a)
	aClientState :: Simple Lens a (Maybe ClientState)

initEndData :: EndState a => Assets -> EndData a
initEndData assets = EndData
	{	_endWinnerLabel = (initLabel (StaticString "Winner:") (bright green) Nothing) {_labelScale = 4}
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
		[	label a (aEnd.endWinnerLabel) ((10,10), (40,45))
		]
	,	(initBox ((25, 50), (630, 550))) <++ -- Rank table
		if isJust $ a^.aClientState
			then concatMap rankLabel $ zip [0..] $ sortBy rankCompare $ game^.gameRanks
			else []
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aEnd.endContinueButton) id ((80,50),(185,28))
		]
	]
	where
		game = a^.aClientState.(to fromJust).clientGame
		rankCompare (_, r) (_, r') = case game^.gameGameMode of
			_ -> compare r r'
		rankLabel (i, (player, rank)) =
			[	label a ((to (\_ -> rank)).(to myLabel)) ((25, 525 - (25 + i * 50)), (25, 25))
			,	label a ((to (\_ -> player)).(to myLabel)) ((50, 525 - (25 + i * 50)), (550, 25))
			]
		myLabel n = (initLabel (StaticString $ show n) (bright green) Nothing) {_labelScale = 2}

timeEnd :: EndState a => Float -> a -> a
timeEnd _ = id

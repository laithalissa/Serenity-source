module Serenity.Game.Client.ClientState
(	Game(..)
,	initialize
,	render
,	handleInput
,	step
,	ClientState(..)
,	UIState(..)
) where

import Graphics.Gloss.Data.Picture(Picture)
import Graphics.Gloss.Interface.Pure.Game(Event)

import Serenity.Game.Client.Assets(Assets)
import qualified Serenity.Game.Client.Graphics as Graphics
import qualified Serenity.Game.Client.InputFilter  as InputFilter

import Serenity.Game.Model.GameState (GameState)
import qualified Serenity.Game.Model.GameState as GameState

import Serenity.Game.Model.ClientMessage(ClientMessage(..))
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.GameMap(GameMap)



import Serenity.Game.Client.UIState

data ClientState = ClientState
	{ messages :: [String] -- XXX should be Messages
	, uiState :: UIState ClientState
	-- gameState :: GameState
	}



data Game = Game
	{	gameState :: GameState
	,	graphics :: Graphics.Graphics
	,	inputFilter :: InputFilter.InputFilter
	}
	deriving(Show, Eq)

initialize :: Assets -> Graphics.WindowSize -> GameMap -> Game
initialize assets windowSize gameMap = Game
	{	gameState=firstWorld
	,	graphics=Graphics.initialize firstWorld assets windowSize
	,	inputFilter=InputFilter.initialize
	}
	where
		firstWorld = GameState.initialize gameMap

render :: Game -> Picture
render game = Graphics.render (gameState game) (graphics game)

handleInput :: Event -> Game -> Game
handleInput event game =
	case (InputFilter.handleInput event . inputFilter) game of
		(Just clientMessage, newInputFilter) -> case clientMessage of
			ClientMessageGraphics graphicsMessage ->
				game{graphics=((Graphics.handleMessage graphicsMessage . graphics) game)}
			ClientMessageWorld worldMessage ->
					game{gameState=((GameState.handleMessage worldMessage . gameState) game)}
		(Nothing, newInputFilter) -> game{inputFilter=newInputFilter}

step :: TimeDuration -> Game -> Game
step timeDelta game = game{gameState=(GameState.step timeDelta (gameState game))}



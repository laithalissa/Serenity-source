module Serenity.Game.Client.Controller

where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Game (Event)

import Serenity.Game.Client.ClientMessage (ClientMessage(..))
import Serenity.Game.Client.ClientState (ClientState(..), UIState(..), windowSize)
import qualified Serenity.Game.Client.InputFilter as InputFilter
import qualified Serenity.Game.Client.GUI as GUI

import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap)
import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Sheen.View

render :: ClientState -> Picture
render clientState = GUI.render (gameState clientState) (uiState clientState) (assets clientState)

handleInput :: Event -> ClientState -> ClientState
handleInput event clientState =
	case InputFilter.handleInput event clientState of
		[] -> clientState
		clientMessages -> handleMessages clientMessages clientState

handleMessages :: [ClientMessage] -> ClientState -> ClientState
handleMessages [] clientState = clientState
handleMessages (m:ms) clientState = case m of
			ClientMessageGUI guiMessage -> handleMessages ms $
				clientState { uiState = GUI.handleMessage guiMessage (uiState clientState) }
			ClientMessageCommand command -> handleMessages ms $
				clientState { commands = (commands clientState) ++ [command] }

initUIState :: GameState -> UIState ClientState
initUIState gameState = UIState
	{	views = mainView
	,	viewPort = ((0,0), 1) --(0, 0, width, height)
	}
	where
		(width, height) = gameMapSize $ gameStateGameMap gameState

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

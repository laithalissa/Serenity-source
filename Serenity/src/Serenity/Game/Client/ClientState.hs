module Serenity.Game.Client.ClientState
(	ClientState(..)
,	initialize
,	render
,	handleInput
) where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Game (Event)

import Serenity.Network.Message (Command)

import Serenity.Game.Client.Common

import Serenity.Game.Client.Assets (Assets)

import qualified Serenity.Game.Client.GUI as GUI

import Serenity.Game.Client.InputFilter (InputFilter)
import qualified Serenity.Game.Client.InputFilter as InputFilter

import Serenity.Game.Client.UIState (UIState(..))

import Serenity.Game.Shared.Model.GameState (GameState, gameMap)
import qualified Serenity.Game.Shared.Model.GameState as GameState

import Serenity.Game.Shared.Model.ClientMessage (ClientMessage(..))
import Serenity.Game.Shared.Model.Common
import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Sheen.View


-- | Represents the state of the client including the current game state
-- and GUI's state
data ClientState = ClientState
	{	gameState :: GameState         -- ^ State of the game world, e.g. ship positions
	,	uiState :: UIState ClientState -- ^ State of the GUI, e.g. view hierarchy
	,	commands :: [Command]          -- ^ List of commands to send to the server
	,	inputFilter :: InputFilter
	,	assets :: Assets
	}

-- | Create the initial client state
initialize ::
	Assets                 -- ^ Assets
	-> GameMap             -- ^ Map
	-> ClientState
initialize assets gameMap = ClientState
	{	gameState = game
	,	uiState = initUIState game
	,	commands = []
	,	inputFilter = InputFilter.initialize
	,	assets = assets
	}
	where
		game = GameState.initialize gameMap

render :: ClientState -> Picture
render clientState = GUI.render (gameState clientState) (uiState clientState) (assets clientState)

handleInput :: Event -> ClientState -> ClientState
handleInput event game =
	case (InputFilter.handleInput event . inputFilter) game of
		(Just clientMessage, newInputFilter) -> case clientMessage of
			ClientMessageGraphics guiMessage ->
				game { uiState = GUI.handleMessage guiMessage . uiState $ game }
			ClientMessageWorld worldMessage ->
					game { gameState = GameState.handleMessage worldMessage . gameState $ game }
		(Nothing, newInputFilter) -> game { inputFilter = newInputFilter }

initUIState :: GameState -> UIState ClientState
initUIState gameState = UIState
	{	views = mainView
	,	viewPort = (0, 0, width, height)
	}
	where
		(width, height) = gameMapSize $ gameMap gameState

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

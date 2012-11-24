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
import Serenity.Game.Client.ClientMessage (ClientMessage(..))
import qualified Serenity.Game.Client.GUI as GUI
import Serenity.Game.Client.InputFilter (InputFilter)
import qualified Serenity.Game.Client.InputFilter as InputFilter
import Serenity.Game.Client.UIState (UIState(..))

import Serenity.Game.Shared.Model.Common
import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap)
import qualified Serenity.Game.Shared.Model.GameState as GameState
import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Sheen.View


-- XXX ONLY FOR THE EXAMPLE GAMESTATE
import Serenity.Game.Shared.Model.Entity
import qualified Data.Set as Set
-- XXX

-- | Represents the state of the client including the current game state
-- and GUI's state
data ClientState = ClientState
	{	gameState :: GameState         -- ^ State of the game world, e.g. ship positions
	,	uiState :: UIState ClientState -- ^ State of the GUI, e.g. view hierarchy
	,	commands :: [Command]          -- ^ List of commands to send to the server
	,	inputFilter :: InputFilter
	,	assets :: Assets
	,	clientName :: OwnerId
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
	,	clientName = "test"
	}
	where
		-- XXX EXAMPLE GAMESTATE
		-- game = GameState.initialize gameMap
		game = GameState.GameState
			{	GameState.gameStateGameMap = gameMap
			,	GameState.gameStateEntities = Set.singleton $ GameEntity
					{	entityId = 1
					,	ownerId = "test"
					,	entity = Ship 0 (50, 50) (0, 0) (0, 0) StayStillOrder
					}
			}

render :: ClientState -> Picture
render clientState = GUI.render (gameState clientState) (uiState clientState) (assets clientState)

handleInput :: Event -> ClientState -> ClientState
handleInput event game =
	case InputFilter.handleInput event (gameState game) (clientName game) (inputFilter game) of
		([], newInputFilter) -> game { inputFilter = newInputFilter }
		(clientMessages, newInputFilter) -> handleMessages clientMessages game

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
	,	viewPort = (0, 0, width, height)
	}
	where
		(width, height) = gameMapSize $ gameStateGameMap gameState

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

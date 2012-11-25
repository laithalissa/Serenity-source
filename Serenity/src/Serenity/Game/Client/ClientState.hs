module Serenity.Game.Client.ClientState
(	ClientState(..)
,	UIState(..)
,	initialize
) where

import Serenity.Game.Client.Common
import Serenity.Game.Client.Assets (Assets)

import Serenity.Game.Shared.Model.Common
import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap)
import qualified Serenity.Game.Shared.Model.GameState as GameState
import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Network.Message (Command)

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
	,	assets :: Assets
	,	clientName :: OwnerId
	}

data UIState a = UIState
	{	views :: View a
	,	viewPort :: ViewPort
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

initUIState :: GameState -> UIState ClientState
initUIState gameState = UIState
	{	views = mainView
	,	viewPort = (0, 0, width, height)
	}
	where
		(width, height) = gameMapSize $ gameStateGameMap gameState

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

module Serenity.Game.Client.ClientState
(	ClientState(..)
,	UIState(..)
,	initialize
,	ViewPort
,	ViewPortMove
,	ViewPortZoom
,	mapLocationFromView
,	windowSize
) where

import Serenity.Game.Client.Assets (Assets)

import Serenity.Game.Shared.Model.Common
import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap, exampleGameState)
import qualified Serenity.Game.Shared.Model.GameState as GameState
import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Network.Message (Command)

import Serenity.Sheen.View

-- | The size of the Gloss window
windowSize :: (Int, Int)
windowSize = (1024, 768)

-- | The view port is the area of the game world that is being viewed
-- by the client. ((x, y), zoom)
type ViewPort = ((Float, Float), Float)

-- | A change in the view port's x and y coordinates
type ViewPortMove = (Float, Float)

-- | A change to the view port's zoom level
type ViewPortZoom = Float

-- | Convert a view port location into an in-game map location
mapLocationFromView ::
	Location    -- ^ Location within the view port
	-> ViewPort -- ^ View port
	-> Size     -- ^ Size of the map
	-> Location

mapLocationFromView (x, y) ((vx, vy), vz) (w, h) = (mapX, mapY)
	where
		mapX = (-(vx*(1-s)) - (ww/2) + x)/s
		mapY = (-(vy*(1-s)) - (wh/2) + y)/s

		ww = fromIntegral $ fst windowSize
		wh = fromIntegral $ snd windowSize
		normScale = ((min ww wh) / (max w h))
		s = vz * normScale

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
	Assets         -- ^ Assets
	-> GameMap     -- ^ Map
	-> OwnerId     -- ^ Player's name
	-> ClientState
initialize assets gameMap name = ClientState
	{	gameState = game
	,	uiState = initUIState game
	,	commands = []
	,	assets = assets
	,	clientName = name
	}
	where
		-- game = GameState.initialize gameMap
		game = exampleGameState

initUIState :: GameState -> UIState ClientState
initUIState gameState = UIState
	{	views = mainView
	,	viewPort = ((width/2, height/2), zoom)
	}
	where
		(width, height) = gameMapSize $ gameStateGameMap gameState
		zoom = 1

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

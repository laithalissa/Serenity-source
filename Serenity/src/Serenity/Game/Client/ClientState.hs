{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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


import Serenity.Game.Shared.Model.Common
--import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap, demoGameState)
--import qualified Serenity.Game.Shared.Model.GameState as GameState
--import Serenity.Game.Shared.Model.GameMap (GameMap, gameMapSize)

import Serenity.Extensions.Vinyl

import Serenity.Model.Game
import Serenity.Model.Sector

import Serenity.Network.Message (Command)
import Serenity.Sheen.View
import Serenity.Game.Client.Assets (Assets)
import Serenity.Game.Client.KeyboardState

-- | The size of the Gloss window
windowSize :: (Int, Int)
windowSize = (1024, 768)

-- | The view port is the area of the game world that is being viewed
-- by the client. ((x, y), zoom)
type ViewPort = ((Double, Double), Double)

-- | A change in the view port's x and y coordinates
type ViewPortMove = (Double, Double)

-- | A change to the view port's zoom level
type ViewPortZoom = Double

-- | Convert a view port location into an in-game map location
--mapLocationFromView ::
--	Location    -- ^ Location within the view port
--	-> ViewPort -- ^ View port
--	-> Size     -- ^ Size of the map
--	-> Location

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
newtype ClientState = ClientState (Rec
	[	"game"          ::: Game                -- ^ State of the game world, e.g. ship positions
	,	"uiState"       ::: UIState ClientState -- ^ State of the GUI, e.g. view hierarchy
	,	"keyboardState" ::: KeyboardState       -- ^ What keys are down and in what order they went down
	,	"commands"      ::: [Command]           -- ^ List of commands to send to the server
	,	"assets"        ::: Assets
	,	"clientName"    ::: OwnerId
	])
_game          = Field :: "game"          ::: Game
_uiState       = Field :: "uiState"       ::: UIState ClientState
_keyboardState = Field :: "keyboardState" ::: KeyboardState
_commands      = Field :: "commands"      ::: [Command]
_assets        = Field :: "assets"        ::: Assets
_clientName    = Field :: "clientName"    ::: OwnerId

data UIState a = UIState
	{	uiView :: View a
	,	uiViewPort :: ViewPort
	}

-- | Create the initial client state
initialize ::
	Assets         -- ^ Assets
	-> Sector     -- ^ Map
	-> OwnerId     -- ^ Player's name
	-> ClientState
initialize assets gameMap name = ClientState $
	    _game          =: game
	<+> _uiState       =: initUIState game
	<+> _keyboardState =: emptyKeyboardState
	<+> _commands      =: []
	<+> _assets        =: assets
	<+> _clientName    =: name
	
	where
		-- game = GameState.initialize gameMap
		game = defaultGame --demoGameState

initUIState :: Game -> UIState ClientState
initUIState game = UIState
	{	uiView = mainView
	,	uiViewPort = ((width/2, height/2), zoom)
	}
	where
		(width, height) = game ^. ((rLens _sector) . (rLens _size)) -- gameMapSize $ gameStateGameMap game
		zoom = 1

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)

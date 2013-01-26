{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.Client.ClientState where

import Serenity.Game.Client.Assets (Assets)

import Serenity.Model.Message (Command)
import Serenity.Game.Client.KeyboardState
import Serenity.Sheen.View

import Serenity.Model
import Control.Lens

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

type Location = (Double, Double)
type Size = (Double, Double)

-- | Convert a view port location into an in-game map location
mapLocationFromView
	:: Location -- ^ Location within the view port
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
	{	_clientGame :: Game                   -- ^ State of the game world, e.g. ship positions
	,	_clientUIState :: UIState ClientState -- ^ State of the GUI, e.g. view hierarchy
	,	_clientKeyboardState :: KeyboardState -- ^ What keys are down and in what order they went down
	,	_clientCommands :: [Command]          -- ^ List of commands to send to the server
	,	_clientAssets :: Assets
	,	_clientOwnerID :: OwnerID
	}

data UIState a = UIState
	{	_views :: View a
	,	_viewport :: ViewPort
	}

makeLenses ''UIState
makeLenses ''ClientState

-- | Create the initial client state
initClientState
	:: Assets      -- ^ Assets
	-> OwnerID     -- ^ Player's id
	-> ClientState
initClientState assets ownerID = ClientState
	{	_clientGame = game
	,	_clientUIState = initUIState game
	,	_clientKeyboardState = emptyKeyboardState
	,	_clientCommands = []
	,	_clientAssets = assets
	,	_clientOwnerID = ownerID
	}
	where
		game = demoGame

initUIState :: Game -> UIState ClientState
initUIState game = UIState
	{	_views = mainView
	,	_viewport = ((width/2, height/2), zoom)
	}
	where
		(width, height) = game^.gameSector.sectorSize
		zoom = 1

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)
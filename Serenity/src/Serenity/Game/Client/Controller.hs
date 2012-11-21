module Serenity.Game.Client.Controller
	( UIEvent(..)
	, initWorld
	, drawWorld
	, newWorldFromEvent
	)
where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game (Event(..))

import Serenity.Game.Client.World

import Serenity.Sheen.UIEvent
import Serenity.Sheen.View

initWorld :: World
initWorld = World [] mainView

drawWorld :: World -> IO Picture
drawWorld world = return $ drawView (uiState world) world

newWorldFromEvent :: Event -> World -> World
newWorldFromEvent event world = handleViewEvent event mainView world

mainView :: View World
mainView = (makeView (0, 1024, 0, 768))
	{ viewID = "main"
	, subviews = [menuView, gameView]
	, background = Just black
	}

menuView :: View World
menuView = (makeView (0, 100, 0, 768))
	{ viewID = "menu"
	, background = Just red
	}

gameView :: View World
gameView = (makeView (100, 1024, 0, 768))
	{ viewID = "game"
	, background = Just green
	, eventHandler = Just (\_ world -> world { uiState = changeView "game" (\v -> v { background = Just blue }) (uiState world) })
	}


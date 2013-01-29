
module Serenity.Game.Client.MenuStates.MainMenuState where

import StateSwitcher
import Serenity.Sheen.View
import Graphics.Gloss.Data.Color

initialize = 
	MainMenu
	{	options=[	"Start Game"
			,	"Games List"
			,	"Exit"
			]
	,	currentOption=0
	}	

data MainMenu = 
	MainMenu
	{	options :: [String]
	,	currentOption :: Int
	} deriving(Show, Eq)


initializeMainMenu = mm_view{ subviews = []
	,	zIndex = 0
	,	background = Just black
	,	depict = Nothing
	,	eventHandler = Nothing
	} where mm_view = makeView "menu-mainMenu" (0, 500, 0, 500)

-- instance AppState MainMenu where
-- 	stateUpdate delta mainMenu = Just (wrap mainMenu, Update)
-- 	stateRender mainMenu = pictures 

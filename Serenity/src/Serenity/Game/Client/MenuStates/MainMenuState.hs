
module Serenity.Game.Client.MenuStates.MainMenuState where

import StateSwitcher


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



instance AppState MainMenu where
	stateUpdate delta mainMenu = Just (wrap mainMenu, Update)
	stateRender mainMenu = pictures 


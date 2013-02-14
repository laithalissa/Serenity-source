{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import qualified Serenity.Game.Client.Assets as Assets

import Graphics.Gloss.Interface.IO.Game
import Control.Lens


data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode   :: ApplicationMode
	,	_appAssets :: Assets.Assets
	,	_appMenus  :: [String]
	,	_appCount  :: Int
	,	_appMenuLabel :: Button ApplicationController ApplicationMode
	,	_appMenuButton :: Button ApplicationController ApplicationMode
	,	_appGameLabel :: Label ApplicationController
	}

data ApplicationMode = MainMenu | Game

makeLenses ''ApplicationController

initApplicationController assets = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode   = MainMenu
	,	_appAssets = assets
	,	_appMenus  = ["Menu 1", "Menu 2"]
	,	_appCount  = 0
	,	_appMenuLabel  = initButton (initLabel (StringLabel "Main Menu") black (Just red)) (initLabel (StringLabel "Main Menu") black (Just blue)) []
	,	_appMenuButton = initButton (initLabel (StringLabel "Main Menu") black (Just red)) (initLabel (StringLabel "Main Menu") black (Just blue)) [(ButtonEvent LeftButton Up (Modifiers Up Up Up), \_ -> Game)]
	,	_appGameLabel  = initLabel (StringLabel "Game") black (Just green)
	}

instance ViewController ApplicationController where
	getView app = case app^.appMode of 
		MainMenu -> viewOne app
		Game     -> viewTwo app

viewOne app = 
	(initView ((0, 0), (1024, 768)))
	<++ 
	[	button app appMenuLabel appMode ((100,100),(16,110))
	,	button app appMenuButton appMode ((100,400),(16,110))
	]

viewTwo app = 
	(initView ((0, 0), (1024, 768)))
	<++ 
	[	label app appGameLabel ((100,100),(16,110))
	]

gui = do
	assets <- Assets.initialize
	playIOZero
		(InWindow "Project Serenity" (1024, 768) (0, 0))
		black
		30
		(initApplicationController assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		(\_ -> \a -> return a)
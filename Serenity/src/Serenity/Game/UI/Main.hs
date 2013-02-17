module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Menu
import Serenity.Game.UI.Splash
import Serenity.External

import Control.Lens

data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode        :: ApplicationMode
	,	_appAssets      :: Assets
	,	_appSplashData  :: SplashData ApplicationController
	,	_appMenuData    :: MenuData ApplicationController
	}

makeLenses ''ApplicationController

initApplicationController assets = ApplicationController
	{	_appViewGlobals   = initGlobals
	,	_appMode          = Splash
	,	_appAssets        = assets
	,	_appSplashData    = initSplashData appSplashData assets
	,	_appMenuData      = initMenuData appMenuData assets
	}

instance ViewController ApplicationController where
	getView app = case app^.appMode of 
		Splash -> viewSplash app appSplashData appAssets appMode
		Menu   -> viewMenu   app appMenuData appAssets
		Game   -> undefined
	updateTime dt app = case app^.appMode of 
		Splash -> timeSplash appSplashData appMode dt app
		Menu   -> timeMenu appMenuData appMode dt app
		Game   -> app

gui = do
	assets <- initAssets
	playIOZero
		(InWindow "Project Serenity" (1024, 768) (0, 0))
		black
		30
		(initApplicationController assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		(\dt -> \a -> return $ updateTime dt a)

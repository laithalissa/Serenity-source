{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Menu
import Serenity.Game.UI.Splash
import qualified Serenity.Game.Client.Assets as Assets

import Control.Lens

data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode        :: ApplicationMode
	,	_appSplashData  :: SplashData ApplicationController
	,	_appMenuData    :: MenuData ApplicationController
	}

makeLenses ''ApplicationController

initApplicationController assets = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode        = Splash
	,	_appSplashData  = initSplashData appSplashData
	,	_appMenuData    = initMenuData appMenuData
	}

instance ViewController ApplicationController where
	getView app = case app^.appMode of 
		Splash -> viewSplash app appSplashData
		Menu   -> viewMenu   app appMenuData
		Game   -> undefined
	updateTime dt app = case app^.appMode of 
		Splash -> timeSplash appSplashData appMode dt app
		Menu   -> timeMenu appMenuData appMode dt app
		Game   -> app

gui = do
	assets <- Assets.initialize
	playIOZero
		(InWindow "Project Serenity" (1024, 768) (0, 0))
		black
		30
		(initApplicationController assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		(\dt -> \a -> return $ updateTime dt a)

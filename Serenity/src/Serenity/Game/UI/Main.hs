{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import qualified Serenity.Game.Client.Assets as Assets

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

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

data ApplicationController = ApplicationController
	{	_appMode   :: ApplicationMode
	,	_appAssets :: Assets.Assets
	,	_appMenus  :: [String]
	,	_appCount  :: Int
	,	_appMenuLabel :: Label
	,	_appGameLabel :: Label
	}

data ApplicationMode = MainMenu | Game

initApplicationController assets = ApplicationController
	{	_appMode  = MainMenu
	,	_appAssets = assets
	,	_appMenus = ["Menu 1", "Menu 2"]
	,	_appCount = 0
	,	_appMenuLabel = initLabel "Main Menu" black (Just red)
	,	_appGameLabel = initLabel "Game" black (Just green)
	}

makeLenses ''ApplicationController

viewOne app = 
	((initView ((0, 0), (1024, 768))) { _viewEventHandler= Just $ \_ -> appMode .~ Game $ app })
	<++ 
	[	label app appMenuLabel ((100,100),(16,110))
	,	label app appMenuLabel ((100,400),(16,110))
	]

viewTwo app = 
	(initView ((0, 0), (1024, 768)))
	<++ 
	[	label app appGameLabel ((100,100),(16,110))
	]

instance ViewController ApplicationController where
	getView app = case app^.appMode of 
		MainMenu -> viewOne app
		Game     -> viewTwo app
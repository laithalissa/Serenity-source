module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Menu
import Serenity.Game.UI.Splash
import Serenity.Game.UI.Host
import Serenity.External
import Serenity.Model

import Control.Lens
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode        :: ApplicationMode
	,	_appAssets      :: Assets
	,	_appSplashData  :: SplashData ApplicationController
	,	_appMenuData    :: MenuData ApplicationController
	,	_appHostData    :: HostData ApplicationController
	,	_appGameData    :: TMVar (Maybe Game)
	}

makeLenses ''ApplicationController

initApplicationController gameRef assets = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode        = Splash
	,	_appAssets      = assets
	,	_appSplashData  = initSplashData assets
	,	_appMenuData    = initMenuData assets
	,	_appHostData    = initHostData assets
	,	_appGameData    = gameRef
	}

instance ViewController ApplicationController where
	getView app = case app^.appMode of 
		Splash -> viewSplash app appSplashData appAssets appMode
		Menu   -> viewMenu app appMenuData appAssets appMode
		Host   -> viewHost app appHostData appAssets appMode
		Join   -> undefined
		Lobby  -> undefined
		Play   -> undefined
		Quit   -> (initView ((0, 0), (1024, 750)))
	updateTime dt app = case app^.appMode of 
		Splash -> timeSplash appSplashData appMode dt app
		Menu   -> timeMenu appMenuData appMode dt app
		Host   -> timeHost appHostData appMode dt app
		Join   -> app
		Lobby  -> app
		Play   -> app
		Quit   -> error "Quit!"

gui = do
	assets  <- initAssets
	gameRef <- newTMVarIO Nothing
	forkIO (serverThread gameRef)
	playIOZero
		(InWindow "Project Serenity" (1024, 750) (0, 0))
		black
		30
		(initApplicationController gameRef assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		handleMainTime

handleMainTime :: Float -> ApplicationController -> IO ApplicationController
handleMainTime dt = execStateT $ do
	app <- id <%= updateTime dt
	gameRef <- use appGameData
	case app^.appHostData.hostServerGame of
		Starting   -> runServer' gameRef
		Stopping _ -> stopServer' gameRef
		_          -> return ()
	where
	runServer' gameRef = do
		gameBuilder <- liftIO makeDemoGameBuilder
		status <- liftIO.atomically $ takeTMVar gameRef
		g <- return $ case status of 
			Nothing -> demoGame gameBuilder
			Just  g -> g
		appHostData.hostServerGame .= (Running g)
		liftIO.atomically $ putTMVar gameRef (Just g)

	stopServer' gameRef = do
		_ <- liftIO.atomically $ swapTMVar gameRef Nothing
		appHostData.hostServerGame .= Stopped

serverThread :: (TMVar (Maybe Game)) -> IO ()
serverThread ref = return ()
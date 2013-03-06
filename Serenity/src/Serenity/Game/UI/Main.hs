{-# LANGUAGE NoMonomorphismRestriction #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Menu
import Serenity.Game.UI.Splash
import Serenity.Game.UI.Host
import Serenity.Game.UI.Join
import Serenity.Game.UI.Lobby
import Serenity.Game.UI.Play
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Model

import Control.Lens
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode        :: ApplicationMode
	,	_appAssets      :: Assets
	,	_appSplashData  :: SplashData ApplicationController
	,	_appMenuData    :: MenuData ApplicationController
	,	_appHostData    :: HostData ApplicationController
	,	_appJoinData    :: JoinData ApplicationController
	,	_appLobbyData   :: LobbyData ApplicationController
	,	_appPlayData    :: PlayData ApplicationController
	,	_appGameData    :: TMVar (Maybe Game)
	,	_appClientState :: Maybe ClientState
	}

makeLenses ''ApplicationController

initApplicationController gameRef assets = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode        = Splash
	,	_appAssets      = assets
	,	_appSplashData  = initSplashData assets
	,	_appMenuData    = initMenuData assets
	,	_appHostData    = initHostData appHostData assets
	,	_appJoinData    = initJoinData appJoinData assets
	,	_appLobbyData   = initLobbyData appLobbyData assets
	,	_appPlayData    = initPlayData appPlayData assets
	,	_appGameData    = gameRef
	,	_appClientState = Nothing
	}

appServerString   = appJoinData.joinAddress
appServerPortJoin = appJoinData.joinPort
appServerPortHost = appHostData.hostPort

instance ViewController ApplicationController where
	globals = appViewGlobals
	getView app = case app^.appMode of 
		Splash -> viewSplash app appSplashData appAssets appMode
		Menu   -> viewMenu app appMenuData appAssets appMode
		Host   -> viewHost app appHostData appAssets appMode
		Join   -> viewJoin app appJoinData appAssets appMode
		Lobby  -> viewLobby app appLobbyData appClientState appAssets appMode
		Play   -> viewPlay app appPlayData appClientState appAssets appMode
		Quit   -> (initView ((0, 0), (1024, 750)))
	updateTime dt app = case app^.appMode of 
		Splash -> timeSplash appSplashData appMode dt app
		Menu   -> timeMenu appMenuData appMode dt app
		Host   -> timeHost appHostData appMode dt app
		Join   -> timeJoin appJoinData appMode dt app
		Lobby  -> timeLobby appLobbyData appClientState appMode dt app
		Play   -> timePlay appPlayData appClientState appMode dt app
		Quit   -> app -- Quit handled by handleMainTime below

gui = do
	assets  <- initAssets
	gameRef <- newTMVarIO Nothing
	forkIO (forever $ serverThread gameRef)
	playIOZero
		(InWindow "Project Serenity" (1024, 750) (0, 0))
		black
		50
		(initApplicationController gameRef assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		handleMainTime

handleMainTime :: Float -> ApplicationController -> IO ApplicationController
handleMainTime dt = execStateT $ do
	app <- get
	modify $ updateTime dt
	case (app^.appMode) of 
		Host  -> timeHostIO appHostData appGameData dt
		Lobby -> timeLobbyIO appLobbyData appClientState appServerString appServerPortJoin dt
		Play  -> timePlayIO appPlayData appClientState dt
		Quit  -> liftIO exitSuccess
		_     -> return ()

serverThread :: TMVar (Maybe Game) -> IO ()
serverThread ref = do
	mGame <- atomically $ takeTMVar ref
	case mGame of
		Just game -> return ()
		Nothing -> return ()
	atomically $ putTMVar ref mGame

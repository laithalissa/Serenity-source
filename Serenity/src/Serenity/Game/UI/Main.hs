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

import Control.Lens
import Control.Monad.State
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
	,	_appClientState :: Maybe ClientState
	,	_appPort        :: String
	}

makeLenses ''ApplicationController

initApplicationController assets = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode        = Splash
	,	_appAssets      = assets
	,	_appSplashData  = initSplashData assets
	,	_appMenuData    = initMenuData assets
	,	_appHostData    = initHostData appHostData appPort assets
	,	_appJoinData    = initJoinData appJoinData appPort assets
	,	_appLobbyData   = initLobbyData appLobbyData assets
	,	_appPlayData    = initPlayData appPlayData assets
	,	_appClientState = Nothing
	,	_appPort        = "9900"
	}

appServerString   = appJoinData.joinAddress

instance ViewController ApplicationController where
	globals = appViewGlobals
	getView app = case app^.appMode of 
		Splash -> viewSplash app appSplashData appAssets appMode
		Menu   -> viewMenu app appMenuData appAssets appMode
		Host   -> viewHost app appHostData appPort appAssets appMode
		Join   -> viewJoin app appJoinData appPort appAssets appMode
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
	playIOZero
		(InWindow "Project Serenity" (1024, 750) (0, 0))
		black
		50
		(initApplicationController assets)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a & correctFocus)
		handleMainTime

correctFocus = execState $ do
	mode <- use appMode
	when (mode == Play) $ appViewGlobals.globalFocus .= [0]

handleMainTime :: Float -> ApplicationController -> IO ApplicationController
handleMainTime dt = execStateT $ do
	app <- get
	modify $ updateTime dt
	case (app^.appMode) of 
		Host  -> timeHostIO appHostData appPort dt
		Lobby -> timeLobbyIO appLobbyData appClientState appServerString appPort dt
		Play  -> timePlayIO appPlayData appClientState dt
		Quit  -> liftIO exitSuccess
		_     -> return ()

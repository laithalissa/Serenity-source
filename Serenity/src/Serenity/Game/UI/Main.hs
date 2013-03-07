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
	,	_appMenuData    = initMenuData   assets
	,	_appHostData    = initHostData   assets
	,	_appJoinData    = initJoinData   assets
	,	_appLobbyData   = initLobbyData  assets
	,	_appPlayData    = initPlayData   assets
	,	_appClientState = Nothing
	,	_appPort        = "9900"
	}

appServerString   = appJoinData.joinAddress

instance AppState    ApplicationController where {aMode=appMode; aAssets=appAssets}
instance SplashState ApplicationController where {aSplash=appSplashData}
instance MenuState   ApplicationController where {aMenu=appMenuData}
instance HostState   ApplicationController where {aHost=appHostData; aPort=appPort}
instance JoinState   ApplicationController where {aJoin=appJoinData; aPort=appPort}
instance LobbyState  ApplicationController where {aLobby=appLobbyData; aClientState=appClientState; aHostName=appServerString; aPort=appPort}
instance PlayState   ApplicationController where {aPlay=appPlayData; aClientState=appClientState}

instance ViewController ApplicationController where
	globals = appViewGlobals
	getView app = case app^.appMode of 
		Splash -> viewSplash app
		Menu   -> viewMenu   app 
		Host   -> viewHost   app
		Join   -> viewJoin   app
		Lobby  -> viewLobby  app
		Play   -> viewPlay   app 
		Quit   -> (initView ((0, 0), (1024, 750)))
	updateTime dt app = case app^.appMode of 
		Splash -> timeSplash dt app
		Menu   -> timeMenu   dt app
		Host   -> timeHost   dt app
		Join   -> timeJoin   dt app
		Lobby  -> timeLobby  dt app
		Play   -> timePlay   dt app
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
		Host  -> timeHostIO  dt
		Lobby -> timeLobbyIO dt
		Play  -> timePlayIO  dt
		Quit  -> liftIO exitSuccess
		_     -> return ()

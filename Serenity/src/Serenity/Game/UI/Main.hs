{-# LANGUAGE NoMonomorphismRestriction #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Menu
import Serenity.Game.UI.Credits
import Serenity.Game.UI.Splash
import Serenity.Game.UI.Host
import Serenity.Game.UI.Join
import Serenity.Game.UI.Quick
import Serenity.Game.UI.FleetBuilder
import Serenity.Game.UI.Lobby
import Serenity.Game.UI.Play
import Serenity.Game.UI.End
import Serenity.Game.Client.ClientState
import Serenity.Model
import Serenity.External

import Control.Lens
import Control.Monad.State
import Data.Char
import System.Exit
import System.Posix.User
import Data.Map (Map)

data ApplicationController = ApplicationController
	{	_appViewGlobals :: ViewGlobals ApplicationController
	,	_appMode        :: ApplicationMode
	,	_appAssets      :: Assets
	,	_appSplashData  :: SplashData ApplicationController
	,	_appMenuData    :: MenuData ApplicationController
	,	_appCreditsData :: CreditsData ApplicationController
	,	_appHostData    :: HostData ApplicationController
	,	_appJoinData    :: JoinData ApplicationController
	,	_appQuickData   :: QuickData ApplicationController
	,	_appFleetData   :: FleetData ApplicationController
	,	_appLobbyData   :: LobbyData ApplicationController
	,	_appPlayData    :: PlayData ApplicationController
	,	_appEndData     :: EndData ApplicationController
	,	_appClientState :: Maybe ClientState
	,	_appPort        :: String
	,	_appNickName    :: String
	,	_appFleet       :: Fleet
	,	_appShipClasses :: Map String ShipClass
	,	_appWeapons     :: Map String Weapon
	,	_appSystems     :: Map String System
	,	_appMusicTID    :: Maybe ThreadId
	}

makeLenses ''ApplicationController

initApplicationController assets shipClasses weapons systems = ApplicationController
	{	_appViewGlobals = initGlobals
	,	_appMode        = Splash
	,	_appAssets      = assets
	,	_appSplashData  = initSplashData  assets
	,	_appMenuData    = initMenuData    assets
	,	_appCreditsData = initCreditsData assets
	,	_appHostData    = initHostData    assets
	,	_appJoinData    = initJoinData    assets
	,	_appQuickData   = initQuickData   assets
	,	_appFleetData   = initFleetData   assets
	,	_appLobbyData   = initLobbyData   assets
	,	_appPlayData    = initPlayData    assets
	,	_appEndData     = initEndData     assets
	,	_appClientState = Nothing
	,	_appPort        = "9900"
	,	_appNickName    = ""
	,	_appFleet       = demoFleet
	,	_appShipClasses = shipClasses
	,	_appWeapons     = weapons
	,	_appSystems     = systems
	,	_appMusicTID    = Nothing
	}

appServerString = appJoinData.joinAddress

instance AppState     ApplicationController where {aMode=appMode; aAssets=appAssets}
instance SplashState  ApplicationController where {aSplash=appSplashData}
instance MenuState    ApplicationController where {aMenu=appMenuData}
instance CreditsState ApplicationController where {aCredits=appCreditsData}
instance HostState    ApplicationController where {aHost=appHostData; aPort=appPort; aName=appNickName}
instance JoinState    ApplicationController where {aJoin=appJoinData; aPort=appPort; aName=appNickName}
instance QuickState   ApplicationController where {aQuick=appQuickData; aHost=appHostData; aHostName=appServerString; aPort=appPort}
instance FleetState   ApplicationController where {aFleetB=appFleetData; aFleet=appFleet; aShipClasses=appShipClasses; aWeapons=appWeapons; aSystems=appSystems}
instance LobbyState   ApplicationController where {aLobby=appLobbyData; aClientState=appClientState; aHostName=appServerString; aPort=appPort; aName=appNickName; aFleet=appFleet}
instance PlayState    ApplicationController where {aPlay=appPlayData; aClientState=appClientState; aName=appNickName}
instance EndState     ApplicationController where {aEnd=appEndData; aClientState=appClientState}

instance ViewController ApplicationController where
	globals = appViewGlobals
	getView app = case app^.appMode of 
		Splash  -> viewSplash  app
		Menu    -> viewMenu    app 
		Credits -> viewCredits app 
		Host    -> viewHost    app
		Join    -> viewJoin    app
		Quick   -> viewQuick   app
		FleetB  -> viewFleet   app
		Lobby   -> viewLobby   app
		Play    -> viewPlay    app 
		End     -> viewEnd     app
		Quit    -> (initView ((0, 0), (1024, 750)))
	updateTime dt app = case app^.appMode of 
		Splash  -> timeSplash  dt app
		Menu    -> timeMenu    dt app
		Credits -> timeCredits dt app 
		Host    -> timeHost    dt app
		Join    -> timeJoin    dt app
		Quick   -> timeQuick   dt app
		FleetB  -> timeFleet   dt app
		Lobby   -> timeLobby   dt app
		Play    -> timePlay    dt app
		End     -> timeEnd     dt app
		Quit    -> app -- Quit handled by handleMainTime below

gui = do
	mTid      <- forkIO music
	assets    <- initAssets
	userEntry <- getRealUserID >>= getUserEntryForID
	username  <- return $ _head %~ toUpper $ nameValidation $ userName userEntry
	shipClasses <- initAddons shipClassYamlForm
	weapons <- initAddons weaponYamlForm
	systems <- initAddons systemYamlForm
	playIOZero
		(InWindow "Project Serenity" (1024, 750) (0, 0))
		black
		50
		((initApplicationController assets shipClasses weapons systems) & (appNickName .~ username) & (appMusicTID .~ (Just mTid)))
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
		Credits -> timeCreditsIO dt
		Host    -> timeHostIO  dt
		Quick   -> timeQuickIO dt
		Lobby   -> timeLobbyIO dt
		Play    -> timePlayIO  dt
		Quit    -> do
			case app^.appMusicTID of
				Just tid -> liftIO $ killThread tid
				Nothing -> return ()
			liftIO exitSuccess
		_       -> return ()

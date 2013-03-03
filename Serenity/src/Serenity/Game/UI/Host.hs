{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Host where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Model

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM
import Data.Char

data HostData a = HostData
	{	_hostTitleLabel      :: Label a
	,	_hostVersionLabel    :: Label a
	,	_hostStartButton     :: Button a (ServerStatus Game)
	,	_hostStopButton      :: Button a (ServerStatus Game)
	,	_hostBackButton      :: Button a ApplicationMode
	,	_hostPlayButton      :: Button a ApplicationMode
	,	_hostNumPlayersBox   :: TextBoxLabel a
	,	_hostNumPlayers      :: String
	,	_hostServerGame      :: ServerStatus Game
	,	_hostNickName        :: String
	,	_hostNickNameBox     :: TextBoxLabel a
	,	_hostPort            :: String
	,	_hostPortBox         :: TextBoxLabel a
	}

data ServerStatus g = Stopped | Starting | Running g | Stopping g

makeLenses ''HostData

initHostData :: Simple Lens a (HostData a) -> Assets -> HostData a
initHostData aHost assets    = HostData
	{	_hostTitleLabel      = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_hostVersionLabel    = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_hostStartButton     = initMenuButton "Start    >>" startServer
	,	_hostStopButton      = initMenuButton "Stop    <>" stopServer
	,	_hostBackButton      = initMenuButton "<-      Back" (\_ -> Menu)
	,	_hostPlayButton      = initMenuButton "Play      ->" (\_ -> Lobby)
	,	_hostNumPlayersBox   = (initMenuTextBoxLabel "Players:" (aHost.hostNumPlayers)) & (tblPostEdit .~ numPlayersValidation)
	,	_hostNumPlayers      = "2"
	,	_hostServerGame      = Stopped
	,	_hostNickName        = ""
	,	_hostNickNameBox     = (initMenuTextBoxLabel "Name:" (aHost.hostNickName)) & (tblPostEdit .~ nameValidation)
	,	_hostPort            = "9050"
	,	_hostPortBox         = (initMenuTextBoxLabel "Port:" (aHost.hostPort)) & (tblPostEdit .~ portValidation)
	}

startServer hostServer = case hostServer of
	Stopped    -> Starting
	Starting   -> Starting
	Running g  -> Running g
	Stopping _ -> Starting

stopServer hostServer = case hostServer of
	Stopped    -> Stopped
	Starting   -> Stopped
	Running g  -> Stopping g
	Stopping g -> Stopping g

viewHost :: a -> Simple Lens a (HostData a) -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewHost a aHost aAssets aMode = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	label a (aHost.hostTitleLabel) ((30,650),(220,30))
	,	label a (aHost.hostVersionLabel) ((0,0),(100,15))
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aHost.hostPlayButton) aMode ((80,650),(185,28))
		,	button a (aHost.hostBackButton) aMode ((80, 50),(185,28))
		]
	,	(initBox ((20, 545), (650, 56))) <++ -- Name Field
		[	textBoxLabel a (aHost.hostNickNameBox) (aHost.hostNickName) ((14,14),(620,28)) 80
		]
	,	(initBox ((20, 35), (650, 56))) <++ -- Server Buttons
		[	if serverRunning $ a^.aHost
				then (button a (aHost.hostStopButton)  (aHost.hostServerGame) ((491,14),(145,28)))
				else (button a (aHost.hostStartButton) (aHost.hostServerGame) ((491,14),(145,28)))
		,	textBoxLabel a (aHost.hostNumPlayersBox) (aHost.hostNumPlayers) ((14,14),(108,28)) 85
		,	textBoxLabel a (aHost.hostPortBox) (aHost.hostPort) ((236,14),(149,28)) 65
		]
	] ++ if serverRunning $ a^.aHost 
		then return (initBox ((20, 100), (650, 435)))
		else return (initBox ((20, 100), (650, 435)))

serverRunning host = case host^.hostServerGame of
	Running _ -> True
	_ -> False

timeHost :: Simple Lens a (HostData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeHost aHost aMode dt = execState $ do
	a <- get
	host <- use aHost
	numPlayersString <- use (aHost.hostNumPlayers)
	nickNameString   <- use (aHost.hostNickName)
	portString       <- use (aHost.hostPort)
	aHost.hostStartButton.buttonEnabled .= (numPlayersString `notElem` ["", "0"])
	aHost.hostPlayButton.buttonEnabled  .= ((serverRunning host) && (nickNameString /= ""))
	aHost.hostNumPlayersBox.tblEnabled  .= (not $ serverRunning host)
	aHost.hostPortBox.tblEnabled        .= (not $ serverRunning host)

timeHostIO :: Simple Lens a (HostData a) -> Simple Lens a (TMVar (Maybe Game)) -> Float -> StateT a IO ()
timeHostIO aHost aGameData _ = do
	a <- get
	gameRef <- use aGameData
	case a^.aHost.hostServerGame of
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
		aHost.hostServerGame .= (Running g)
		liftIO.atomically $ putTMVar gameRef (Just g)

	stopServer' gameRef = do
		_ <- liftIO.atomically $ swapTMVar gameRef Nothing
		aHost.hostServerGame .= Stopped

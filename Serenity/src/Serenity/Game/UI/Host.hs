{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Host where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.Server.Main
import Serenity.External

import Control.Lens
import Control.Monad.State
import Control.Concurrent

data HostData a = HostData
	{	_hostTitleLabel      :: Label a
	,	_hostVersionLabel    :: Label a
	,	_hostStartButton     :: Button a (ServerStatus ThreadId)
	,	_hostStopButton      :: Button a (ServerStatus ThreadId)
	,	_hostBackButton      :: Button a ApplicationMode
	,	_hostPlayButton      :: Button a ApplicationMode
	,	_hostNumPlayersBox   :: TextBoxLabel a
	,	_hostNumPlayers      :: String
	,	_hostServerGame      :: ServerStatus ThreadId
	,	_hostNickNameBox     :: TextBoxLabel a
	,	_hostPortBox         :: TextBoxLabel a
	}

data ServerStatus g = Stopped | Starting | Running g | Stopping g

makeLenses ''HostData

class AppState a => HostState a where
	aHost :: Simple Lens a (HostData a)
	aPort :: Simple Lens a String
	aName :: Simple Lens a String

initHostData :: HostState a => Assets -> HostData a
initHostData assets    = HostData
	{	_hostTitleLabel      = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_hostVersionLabel    = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_hostStartButton     = (initMenuButton "Start    >>" startServer) & (buttonEnabled .~ startButtonEnabled aHost)
	,	_hostStopButton      = (initMenuButton "Stop    <>" stopServer)
	,	_hostBackButton      = (initMenuButton "<-      Back" (\_ -> Menu))
	,	_hostPlayButton      = (initMenuButton "Play      ->" (\_ -> Lobby)) & (buttonEnabled .~ playButtonEnabled)
	,	_hostNumPlayersBox   = 
			(initMenuTextBoxLabel "Players:" (aHost.hostNumPlayers)) 
			& (tblPostEdit .~ numPlayersValidation) 
			& (tblEnabled .~ not.serverRunning)
	,	_hostNumPlayers      = "1"
	,	_hostServerGame      = Stopped
	,	_hostNickNameBox     = (initMenuTextBoxLabel "Name:" aName) & (tblPostEdit .~ nameValidation)
	,	_hostPortBox         = 
			(initMenuTextBoxLabel "Port:" aPort) 
			& (tblPostEdit .~ portValidation) 
			& (tblEnabled .~ not.serverRunning)
	}

startButtonEnabled aHost a = a^.aHost.hostNumPlayers `notElem` ["", "0"]

playButtonEnabled :: HostState a => a -> Bool
playButtonEnabled a = (serverRunning a) && ((a^.aName) /= "")

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

viewHost :: HostState a => a -> View a
viewHost a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	label a (aHost.hostTitleLabel) ((30,650),(220,30))
	,	label a (aHost.hostVersionLabel) ((0,0),(100,15))
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aHost.hostPlayButton) aMode ((80,650),(185,28))
		,	button a (aHost.hostBackButton) aMode ((80, 50),(185,28))
		]
	,	(initBox ((20, 545), (650, 56))) <++ -- Name Field
		[	textBoxLabel a (aHost.hostNickNameBox) aName ((14,14),(620,28)) 80
		]
	,	(initBox ((20, 35), (650, 56))) <++ -- Server Buttons
		[	if serverRunning a
				then (button a (aHost.hostStopButton)  (aHost.hostServerGame) ((491,14),(145,28)))
				else (button a (aHost.hostStartButton) (aHost.hostServerGame) ((491,14),(145,28)))
		,	textBoxLabel a (aHost.hostNumPlayersBox) (aHost.hostNumPlayers) ((14,14),(108,28)) 85
		,	textBoxLabel a (aHost.hostPortBox) aPort ((236,14),(149,28)) 65
		]
	] ++ if serverRunning a
		then return (initBox ((20, 100), (650, 435)))
		else return (initBox ((20, 100), (650, 435)))

serverRunning :: HostState a => a -> Bool
serverRunning a = case a^.aHost.hostServerGame of
	Running _ -> True
	_ -> False

timeHost :: HostState a => Float -> a -> a
timeHost dt = id

timeHostIO :: HostState a => Float -> StateT a IO ()
timeHostIO _ = do
	a <- get
	case a^.aHost.hostServerGame of
		Starting -> runServer'
		Stopping serverThreadID -> stopServer' serverThreadID
		_          -> return ()
	where
	runServer' = do
		serverPort <- use aPort
		numPlayers <- use (aHost.hostNumPlayers)
		serverThreadID <- liftIO.forkIO $ server (fromIntegral $ read serverPort) (read numPlayers)
		aHost.hostServerGame .= (Running serverThreadID)
		return ()

	stopServer' serverThreadID = do
		liftIO $ killThread serverThreadID
		aHost.hostServerGame .= Stopped

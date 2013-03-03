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
	}

data ServerStatus g = Stopped | Starting | Running g | Stopping g

makeLenses ''HostData

initHostData :: Simple Lens a (HostData a) -> Assets -> HostData a
initHostData aHost assets    = HostData
	{	_hostTitleLabel      = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_hostVersionLabel    = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_hostStartButton     = initMenuButton "Start  >>" startServer
	,	_hostStopButton      = initMenuButton "Stop  <>" stopServer
	,	_hostBackButton      = initMenuButton "<-   Back" (\_ -> Menu)
	,	_hostPlayButton      = initMenuButton "Play    ->" (\_ -> Lobby)
	,	_hostNumPlayersBox   = (initMenuTextBoxLabel "Players:" (aHost.hostNumPlayers)) & (tblPostEdit .~ numPlayersValidation)
	,	_hostNumPlayers      = "2"
	,	_hostServerGame      = Stopped
	}

numPlayersValidation = (\x -> case x of _:_ -> [last x]; _ -> "").(filter isDigit)

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
	{	_viewDepict = Just $ getPicture "background" (a^.aAssets)
	}	<++
	[	label a (aHost.hostTitleLabel) ((30,650),(220,30))
	,	label a (aHost.hostVersionLabel) ((0,0),(100,15))
	,	(initView ((680, 0), (345, 750))) -- Sidebar
		{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
		}	<++
		[	button a (aHost.hostPlayButton) aMode ((80,650),(185,28))
		,	button a (aHost.hostBackButton) aMode ((80, 50),(185,28))
		]
	,	(initView ((20, 35), (650, 56))) -- Server Buttons
		{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
		}	<++
		[	if stopped
				then (button a (aHost.hostStartButton) (aHost.hostServerGame) ((491,14),(145,28)))
				else (button a (aHost.hostStopButton)  (aHost.hostServerGame) ((491,14),(145,28)))
		,	textBoxLabel a (aHost.hostNumPlayersBox) (aHost.hostNumPlayers) ((14,14),(128,28)) 105
		]
	] ++ case a^.aHost.hostServerGame of
		Running g  -> return (initView ((20, 100), (650, 500))) 
			{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
			}
		_ -> []
	where
		stopped = case a^.aHost.hostServerGame of
			Stopped    -> True
			Starting   -> True
			Running g  -> False
			Stopping g -> False

timeHost :: Simple Lens a (HostData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeHost aHost aMode dt = execState $ do
	numPlayersString <- use (aHost.hostNumPlayers)
	aHost.hostStartButton.buttonEnabled .= (numPlayersString `notElem` ["", "0"])

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

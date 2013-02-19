{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Host where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Model

import Control.Lens
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

data HostData a = HostData
	{	_hostTitleLabel    :: Label a
	,	_hostVersionLabel  :: Label a
	,	_hostStartButton   :: Button a (ServerStatus Game)
	,	_hostStopButton    :: Button a (ServerStatus Game)
	,	_hostBackButton    :: Button a ApplicationMode
	,	_hostPlayButton    :: Button a ApplicationMode
	,	_hostServerGame    :: ServerStatus Game
	}

data ServerStatus g = Stopped | Starting | Running g | Stopping g

makeLenses ''HostData

initHostData :: Assets -> HostData a
initHostData assets = HostData
	{	_hostTitleLabel  = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_hostVersionLabel = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_hostStartButton = initMenuButton "Start   >>" startServer
	,	_hostStopButton  = initMenuButton "<>    Stop" stopServer
	,	_hostBackButton  = initMenuButton "<-    Back" (\_ -> Menu)
	,	_hostPlayButton  = initMenuButton "Play    ->" (\_ -> Play)
	,	_hostServerGame  = Stopped
	}

startServer hostServer = case hostServer of
	Stopped    -> Starting
	Starting   -> Starting
	Running g  -> Running g
	Stopping g -> Starting

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
	,	(initView ((680, 0), (345, 750))) 
		{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
		}	<++
		[	button a (aHost.hostPlayButton) aMode ((80,650),(185,28))
		,	button a (aHost.hostBackButton) aMode ((80, 50),(185,28))
		]
	,	(initView ((20, 35), (650, 56))) 
		{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
		}	<++
		[	button a (aHost.hostStartButton) (aHost.hostServerGame) ((451,14),(185,28))
		,	button a (aHost.hostStopButton)  (aHost.hostServerGame) ((14 ,14),(185,28))
		]
	] ++ case a^.aHost.hostServerGame of
		Running g  -> return (initView ((20, 100), (650, 500))) 
			{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
			}
		_ -> []

timeHost :: Simple Lens a (HostData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeHost aData aMode dt a = a

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

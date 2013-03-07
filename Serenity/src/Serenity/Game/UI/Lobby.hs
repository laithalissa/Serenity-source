{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Lobby where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Network.Transport
import Serenity.Network.Utility
import Serenity.Network.Connection

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

data LobbyData a = LobbyData
	{	_lobbyTitleLabel   :: Label a
	,	_lobbyLoadingLabel :: Label a
	,	_lobbyTime :: Float
	}
makeLenses ''LobbyData

class AppState a => LobbyState a where
	aLobby :: Simple Lens a (LobbyData a)
	aClientState :: Simple Lens a (Maybe ClientState)
	aHostName :: Simple Lens a String
	aPort :: Simple Lens a String

initLobbyData :: LobbyState a => Assets -> LobbyData a
initLobbyData assets = LobbyData
	{	_lobbyTitleLabel   = (initLabel (StaticString "Project Serenity") (bright green) Nothing) & (labelScale .~ 6)
	,	_lobbyLoadingLabel = (initLabel (StaticString "Connecting...") buttonColor (Just buttonBackground)) 
			& (labelScale .~ 3.7) & (labelTextOffset .~ (15,18))
	,	_lobbyTime = 0
	}

viewLobby :: LobbyState a => a -> View a
viewLobby a = (initView ((0, 0), (1024, 750))) 
	& (viewDepict .~ background (a^.aAssets))
	<++
	[	label a (aLobby.lobbyTitleLabel) ((30,650),(220,30))
	,	label a (aLobby.lobbyLoadingLabel) ((370,300),(300,70))
	]

timeLobby :: LobbyState a => Float -> a -> a
timeLobby _ = execState $ do
	mClientState <- use aClientState
	case mClientState of 
		Nothing -> return ()
		Just _ -> aMode .= Play

timeLobbyIO :: LobbyState a => Float -> StateT a IO ()
timeLobbyIO dt = do
	aLobby.lobbyTime += dt
	time <- use $ aLobby.lobbyTime
	when (time > 1) $ do
		mClientState <- use aClientState
		aClientState <~ loadClientState mClientState
	where
		loadClientState Nothing = do
			serverHost <- use aHostName
			serverPort <- use aPort
			channels <- liftIO $ connectTo serverHost (fromIntegral $ read serverPort)
			liftIO $ waitUntilConnected (channelConnection channels)
			assets <- liftIO initAssets
			gameBuilder <- liftIO makeDemoGameBuilder
			ownerID <- return 0
			return $ Just $ initClientState assets gameBuilder ownerID channels
		loadClientState x = return x

waitUntilConnected connTVar = do
	connection <- atomically $ readTVar connTVar
	if isConnected connection
		then return ()
		else threadDelay 10000 >> waitUntilConnected connTVar
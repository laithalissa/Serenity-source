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

initLobbyData :: Simple Lens a (LobbyData a) -> Assets -> LobbyData a
initLobbyData aLobby assets = LobbyData
	{	_lobbyTitleLabel   = (initLabel (StaticString "Project Serenity") (bright green) Nothing) & (labelScale .~ 6)
	,	_lobbyLoadingLabel = (initLabel (StaticString "Loading...") buttonColor (Just buttonBackground)) & (labelScale .~ 3.7)
	,	_lobbyTime = 0
	}

viewLobby :: a -> Simple Lens a (LobbyData a) -> Simple Lens a (Maybe ClientState) -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewLobby a aLobby aClientState aAssets aMode = (initView ((0, 0), (1024, 750))) 
	& (viewDepict .~ background (a^.aAssets))
	<++
	[	label a (aLobby.lobbyTitleLabel) ((30,650),(220,30))
	,	label a (aLobby.lobbyLoadingLabel) ((400,320),(220,50))
	]

timeLobby :: Simple Lens a (LobbyData a) -> Simple Lens a (Maybe ClientState) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeLobby _ aMClientState aMode _ = execState $ do
	mClientState <- use aMClientState
	case mClientState of 
		Nothing -> return ()
		Just _ -> aMode .= Play

timeLobbyIO :: Simple Lens a (LobbyData a) -> Simple Lens a (Maybe ClientState) -> Simple Lens a String -> Simple Lens a String -> Float -> StateT a IO ()
timeLobbyIO aLobby aClientState aHostName aPort dt = do
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
			ownerID <- return 1
			return $ Just $ initClientState assets gameBuilder ownerID channels
		loadClientState x = return x

waitUntilConnected connTVar = do
	connection <- atomically $ readTVar connTVar
	if isConnected connection
		then return ()
		else threadDelay 10000 >> waitUntilConnected connTVar
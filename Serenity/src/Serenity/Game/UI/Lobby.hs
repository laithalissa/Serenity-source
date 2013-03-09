{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Lobby where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Model.Fleet
import Serenity.Model.Message
import Serenity.Model.Sector
import Serenity.Network.Transport
import Serenity.Network.Connection

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import qualified Data.Map as M (fromList)

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
	aName :: Simple Lens a String

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
			nickName <- use aName
			(channels, ownerID) <- liftIO $ connectToServer serverHost (fromIntegral $ read serverPort) nickName
			connected <- liftIO $ waitForStarting (channelInbox channels) []
			assets <- liftIO initAssets
			gameBuilder <- liftIO $ createGameBuilder connected
			return $ Just $ initClientState assets gameBuilder ownerID connected channels
		loadClientState x = return x

		waitForStarting inbox connected = do
			m <- atomically $ readTChan inbox
			case m of
				ControlMessage (ControlSetConnected c) -> waitForStarting inbox c
				ControlMessage ControlStarting -> return connected
				_ -> waitForStarting inbox connected

		createGameBuilder clients = makeGameBuilder sectorOne $
			M.fromList $ map (\c -> (fst c, demoFleet)) clients

connectToServer :: String -> PortNumber -> String -> IO (TransportInterface, Int)
connectToServer host port name = do
	channels <- connectTo host port
	waitUntilConnected (channelConnection channels)
	atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlSetName name)
	(myID, connected) <- waitForID (channelInbox channels)
	print $ "My ID: " ++ (show myID) ++ ", Connected: " ++ (show connected)
	atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlReady)
	return (channels, myID)
	where
		waitUntilConnected connTVar = do
			connection <- atomically $ readTVar connTVar
			if isConnected connection
				then return ()
				else threadDelay 10000 >> waitUntilConnected connTVar

		waitForID inbox = do
			ctl <- getMsg inbox
			case ctl of
				ControlYourID myID -> return (myID, [])
				_ -> do
					ctl' <- getMsg inbox
					return (controlID ctl', controlConnected ctl)

		getMsg inbox = do
			msg <- atomically $ readTChan inbox
			case msg of
				ControlMessage m -> return m
				_ -> getMsg inbox

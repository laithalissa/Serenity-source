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
import qualified Data.Map as M

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
	aFleet :: Simple Lens a Fleet

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
	time <- aLobby.lobbyTime <+= dt
	when (time > 1) $ aClientState <~ (use aClientState >>= loadClientState)
	where
		loadClientState Nothing = do
			serverHost <- use aHostName
			serverPort <- use aPort
			nickName <- use aName
			(channels, ownerID) <- liftIO $ connectToServer serverHost (fromIntegral $ read serverPort) nickName
			fleet <- use aFleet
			liftIO $ sendFleet channels ownerID fleet
			(connected, fleets) <- liftIO $ waitForStart channels [] M.empty
			assets <- liftIO initAssets
			gameBuilder <- liftIO $ makeGameBuilder sectorTwo fleets
			return $ Just $ initClientState assets gameBuilder ownerID connected channels
		loadClientState x = return x

		waitForStart channels connected fleets = do
			if (M.keys fleets) == (map fst connected)
				then atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlReady)
				else return ()
			m <- atomically $ readTChan (channelInbox channels)
			case m of
				ControlMessage (ControlSetConnected c) -> waitForStart channels c fleets
				ControlMessage (ControlSetFleet id fleet) -> waitForStart channels connected (M.insert id fleet fleets)
				ControlMessage ControlStarting -> return (connected, fleets)
				_ -> waitForStart channels connected fleets

		sendFleet channels ownerID fleet = do
			atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlSetFleet ownerID fleet)

connectToServer :: String -> PortNumber -> String -> IO (TransportInterface, Int)
connectToServer host port name = do
	channels <- connectTo host port
	waitUntilConnected (channelConnection channels)
	atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlSetName name)
	(myID, connected) <- waitForID (channelInbox channels)
	print $ "My ID: " ++ (show myID) ++ ", Connected: " ++ (show connected)
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

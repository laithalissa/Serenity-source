{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.Game.Server.Main
(	server
,	connectionPhase
,	sendToClients
,	getCommands
) where

import Serenity.External
import Serenity.Game.Server.ClientData
import Serenity.Model
import Serenity.Model.Wire
import Serenity.Network.Transport
import Serenity.Network.Utility (readTChanUntilEmpty, sendMessages)

import Prelude hiding (id, (.))

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (forever)
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Run the server.
server 
	:: Int   -- ^ Port to listen on.
	-> Int   -- ^ Number of clients to connect.
	-> IO ()
server port clientCount = forever $ do
	print "server started"	
	print $ "waiting for " ++ (show clientCount) ++ " clients to connect..."
	(clients, transport) <- connectionPhase (fromIntegral port) clientCount
	print "all clients connected, starting game"
	gameBuilder' <- createGameBuilder clients
	play 5 clients (demoGame (map (\c -> (clientID c, clientName c)) clients) gameBuilder') commands evolve updates
	closeTransport transport
	print "server finished"
	where
		createGameBuilder clients = makeGameBuilder sectorOne $
			M.fromList $ map (\c -> (clientID c, demoFleet)) clients

-- | Wait for n clients to connect
connectionPhase
	:: PortNumber      -- ^ Port to listen on.
	-> Int             -- ^ Number of clients to connect.
	-> IO ([ClientData], Transport) -- ^ Client connection information.

connectionPhase port clientLimit = do 
	transport <- listen port
	clientChan <- atomically $ newTChan
	transportVar <- atomically $ newTVar transport
	sendAndReceive transportVar
	forkIO $ acceptLoop clientLimit transportVar clientChan
	clients <- connectionPhase' clientChan clientLimit []
	waitForReadies clients []
	transport' <- atomically $ readTVar transportVar
	return (clients, transport')
	where
		acceptLoop limit transportVar chan = do
			channels <- acceptClient transportVar
			atomically $ writeTChan chan channels
			transport <- atomically $ readTVar transportVar
			if M.size (transportConnectionMap transport) >= limit
				then return ()
				else acceptLoop limit transportVar chan

		connectionPhase' chan limit clients = do
			channels <- atomically $ readTChan chan
			name <- getClientName (channelInbox channels)
			let newID = length clients
			let clients' = (ClientData
				{	clientID = newID
				,	clientName = name
				,	clientTransportInterface = channels
				}):clients
			atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlYourID newID)
			sendToClients [ControlMessage $ ControlSetConnected $ map (\c -> (clientID c, clientName c)) clients'] clients'
			if length clients' >= limit
				then return clients'
				else connectionPhase' chan limit clients'

		getClientName chan = do
			msg <- atomically $ readTChan chan
			case msg of
				ControlMessage (ControlSetName name) -> return name
				_ -> getClientName chan

		waitForReadies clients readies = do
			messages <- mapM readTChanUntilEmpty $ map (channelInbox . clientTransportInterface) clients
			let messages' = readies ++ (filter (\m -> case m of ControlMessage ControlReady -> True; _ -> False) $ concat messages)
			if length messages' == length clients
				then sendToClients [ControlMessage ControlStarting] clients
				else waitForReadies clients messages'

-- | Run the server with given update functions.
play :: forall world . (Show world, world ~ Game) 
	=> Int                              -- ^ Number of simulation steps to take for each second of real time.
	-> [ClientData]                     -- ^ Clients
	-> world                            -- ^ The initial world.
	-> ([Command] -> world -> [Update]) -- ^ Function to handle commands from clients.
	-> (UpdateWire (world, world))      -- ^ Wire to step the world one iteration, given the time past
	-> ([Update] -> world -> world)     -- ^ Function to evolve the world from updates.
	-> IO ()

play stepsPerSecond clientDataList initialWorld transform wire updateWorld = do
	time <- getCurrentTime
	playLoop (initialWorld, wire) time
	where 
		playLoop (game, wire) lastTime = do
			commands          <- getCommands clientDataList
			updatesC          <- return $ transform commands game
			filteredUpdates'  <- return $ filteredUpdates updatesC
			game'             <- return $ updateWorld filteredUpdates' game
			newTime           <- getCurrentTime
			time              <- return $ toRational $ diffUTCTime newTime lastTime
			(updatesT, wire') <- return $ runWire wire (fromRational time) (game', game')
			game''            <- return $ gameTime +~ (fromRational time) $ updateWorld updatesT game'
			game'''           <- return $ gameRandom %~ (snd.next) $ game''
			sendToClients (map (\u -> UpdateMessage u 0) (filteredUpdates' ++ updatesT)) clientDataList
			threadDelay $ floor (1000000 / (fromIntegral stepsPerSecond))
			if UpdateGameOver `elem` updatesT
				then return ()
				else playLoop (game''', wire') newTime

-- | Receive commands from the network from all the clients
getCommands :: [ClientData] -> IO [Command]
getCommands clientDataList = do
	messages <- mapM readTChanUntilEmpty inboxes
	return $ concatMap getCommands (concat $ messages)
	where
		inboxes = map (channelInbox . clientTransportInterface) clientDataList
		getCommands (CommandMessage command _ _) = [command]
		getCommands _ = []

-- | Send updates to all clients
sendToClients :: [Message] -> [ClientData] -> IO ()
sendToClients messages clientDataList = mapM_ (\outbox -> sendMessages outbox messages) outboxes
		where
			outboxes = map (channelOutbox . clientTransportInterface) clientDataList


{-# LANGUAGE ExplicitForAll #-}

module Serenity.Game.Server.Main
(	server
,	connectionPhase
,	sendToClients
,	getCommands
) where

import Control.Concurrent (threadDelay)
import Control.Monad.State (runStateT)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Serenity.Network.Message (Command(..), Update(..), Message(..))
import Serenity.Network.Transport
import Serenity.Network.Utility (readTChanUntilEmpty, sendMessages)

import Serenity.Game.Server.ClientData
import Serenity.Game.Shared.GameStateUpdate (manyUpdateGameState)
import Serenity.Game.Shared.Model.GameState (demoGameState)
import Serenity.Game.Server.GameStateTransform (transforms,step)

-- | Run the server.
server :: 
	   Int   -- ^ Port to listen on.
	-> Int   -- ^ Number of clients to connect.
	-> IO ()
server port clientCount = do
	print "server started"	
	print $ "waiting for " ++ (show clientCount) ++ " clients to connect..."
	clients <- connectionPhase (fromIntegral port) clientCount
	print "all clients connected, starting game"
	play 5 clients demoGameState transforms step manyUpdateGameState
	print "server finished"

-- | Wait for n clients to connect
connectionPhase :: 
	   PortNumber      -- ^ Port to listen on.
	-> Int             -- ^ Number of clients to connect.
	-> IO [ClientData] -- ^ Client connection information.

connectionPhase port clientLimit = do 
	transport <- listen port
	(clients, server) <- connectionPhase' clientLimit transport []
	sendAndReceive server
	return clients
	where
		connectionPhase' limit transport clients = do
			(channels, transport') <- runStateT acceptClient transport
			let clients' = (ClientData{clientTransportInterface=channels}):clients
			if length clients' >= limit
				then return (clients', transport')
				else connectionPhase' limit transport' clients'

-- | Run the server with given update functions.
play :: forall world
	.  (Show world) => Int                              -- ^ Number of simulation steps to take for each second of real time.
	-> [ClientData]                     -- ^ Clients
	-> world                            -- ^ The initial world.
	-> ([Command] -> world -> [Update]) -- ^ Function to handle commands from clients.
	-> (Float -> world -> [Update])     -- ^ Function to step the world one iteration, given the past time
	-> ([Update] -> world -> world)     -- ^ Function to evolve the world from updates.
	-> IO ()

play stepsPerSecond clientDataList initialWorld transform step updateWorld = do
	time <- getCurrentTime
	playLoop initialWorld time
	where 
		playLoop gameState lastTime = do
			commands    <- getCommands clientDataList
			updatesC    <- return $ transform commands gameState
			gameState'  <- return $ updateWorld updatesC gameState
			newTime     <- getCurrentTime
			time        <- return $ toRational $ diffUTCTime newTime lastTime
			updatesT    <- return $ step (fromRational time) gameState'
			gameState'' <- return $ updateWorld updatesT gameState'
			sendToClients (updatesC ++ updatesT) clientDataList
			threadDelay $ floor (1000000 / (fromIntegral stepsPerSecond))
			playLoop gameState'' newTime

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
sendToClients :: [Update] -> [ClientData] -> IO ()
sendToClients updates clientDataList = mapM_ (\outbox -> sendMessages outbox messages) outboxes
		where
			outboxes = map (channelOutbox . clientTransportInterface) clientDataList
			messages = map (\u -> UpdateMessage u 0) updates




{-# LANGUAGE ExplicitForAll #-}

module Serenity.Game.Server.Main
(	server
,	connectionPhase
,	sendToClients
,	getCommands
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import Serenity.Network.Transport
import Serenity.Network.Utility
import Serenity.Game.Server.ClientData
import Serenity.Network.Packet
import Serenity.Network.Message(Command(..), Update(..), Message(..))
import Serenity.Game.Shared.GameStateUpdate(manyUpdateGameState)
import Serenity.Game.Shared.Model.GameState(GameState, exampleGameState)
import Serenity.Game.Server.GameStateTransform(transforms,step)

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent(threadDelay)

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
	play 5 clients exampleGameState transforms step manyUpdateGameState
	print "server finished"

-- | Wait for n clients to connect
connectionPhase :: 
	   PortNumber      -- ^ Port to listen on.
	-> Int             -- ^ Number of clients to connect.
	-> IO [ClientData] -- ^ Client connection information.

connectionPhase port clientLimit = do 
	connection <- startListeningIO port
	connectionPhase' clientLimit port connection [] 
	where 
		connectionPhase' clientLimit port connection clientDataList = do
			channels <- listenChannelsIO connection
			clientDataList' <- return $ (ClientData{clientTransportInterface=channels}):clientDataList
			threadDelay 100
			if length clientDataList' >= clientLimit 
				then return clientDataList' 
				else connectionPhase' clientLimit port connection clientDataList'

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
			print gameState''
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
sendToClients updates clientDataList = do 
		--print updates
		mapM (\o -> sendMessages o messages) outboxes
		return ()
		where
			outboxes = map (channelOutbox . clientTransportInterface) clientDataList
			messages = map (\u -> UpdateMessage u 0) updates




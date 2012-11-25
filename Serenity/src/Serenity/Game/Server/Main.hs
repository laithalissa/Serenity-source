{-# LANGUAGE ExplicitForAll #-}

module Serenity.Game.Server.Main
(	main
,	port
,	connectionPhase
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport
import Serenity.Network.Utility
import Serenity.Game.Server.ClientData
import Serenity.Network.Packet
import Serenity.Network.Message(Command(..), Update(..), Message(..))
import Serenity.Game.Shared.GameStateUpdate(manyUpdateGameState)
import Serenity.Game.Shared.Model.GameState(GameState, exampleGameState)
import Serenity.Game.Server.GameStateTransform(transforms,step)

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Concurrent(threadDelay)

port = 9900
clientCount = 1

main = do
	print "server started"
	print $ "waiting for " ++ (show clientCount) ++ " clients to connect..."
	clients <- connectionPhase clientCount port
	print "all clients connected, starting game"
	play 5 clients exampleGameState transforms step manyUpdateGameState
	print "server finished"

	

-- main = do
-- 	connection <- runListen port
-- 	return ()

readInput sock tvar = do
	(packet, client) <- receivePacket sock
	atomically $ writeTVar tvar (packetData packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)

-- | Wait for n clients to connect
connectionPhase :: 
	   Int             -- ^ Number of clients to connect.
	-> PortNumber      -- ^ Port to listen on.
	-> IO [ClientData] -- ^ Client connection information.

connectionPhase clientLimit port = do 
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
	.  Int                              -- ^ Number of simulation steps to take for each second of real time.
	-> [ClientData]                     -- ^ Clients
	-> world                            -- ^ The initial world.
	-> ([Command] -> world -> [Update]) -- ^ Function to handle commands from clients.
	-> (Float -> world -> [Update])     -- ^ Function to step the world one iteration, given the past time
	-> ([Update] -> world -> world)     -- ^ Function to evolve the world from updates.
	-> IO ()

play stepsPerSecond clientDataList initialWorld transform step updateWorld = do
	time <- getPOSIXTime
	playLoop initialWorld time
	where 
		playLoop gameState lastTime = do
			commands    <- getCommands clientDataList
			updatesC    <- return $ transform commands gameState
			gameState'  <- return $ updateWorld updatesC gameState
			newTime     <- getPOSIXTime
			time        <- return $ (realToFrac lastTime) - (realToFrac newTime)
			updatesT    <- return $ step time gameState'
			gameState'' <- return $ updateWorld updatesT gameState'
			return $ sendToClient (updatesC ++ updatesT)
			playLoop gameState'' lastTime

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
sendToClient :: [Update] -> [ClientData] -> IO ()
sendToClient updates clientDataList = do 
		mapM (\o -> sendMessages o messages) outboxes
		return ()
		where
			outboxes = map (channelOutbox . clientTransportInterface) clientDataList
			messages = map (\u -> UpdateMessage u 0) updates




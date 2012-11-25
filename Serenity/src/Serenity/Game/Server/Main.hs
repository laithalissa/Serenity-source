{-# LANGUAGE ExplicitForAll #-}

module Serenity.Game.Server.Main
(	main
,	port
,	updateWorld
,	connectionPhase
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport
import Serenity.Network.Utility
import Serenity.Game.Server.ClientData
import Serenity.Network.Packet
import Serenity.Network.Message
import Serenity.Game.Shared.GameStateUpdate

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Concurrent(threadDelay)

port = 9900

main = print "sup"

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
sendToClient :: [Update] -> [ClientData]
sendToClient = undefined

--play rate initialWord messageUpdate timeUpdate = do
--	connection <- runConnection listen
--	(inbox, outbox, _) <- getTransportChannels connection

--	worldRef <- newIORef initialWord
--	timeRef <- newIORef (getTime)

--	forever $ modifyIORef (loop2 (loop (inbox, outbox)) timeRef) worldRef
--		where
--			loop = network (iterate messageUpdate timeUpdate)
--			loop2 loop timeRef oldWorld  = do
--				newTime <- getTime
--				oldTime <- readIORef timeRef
--				writeIOref timeRef newTime
--				newWorld <- loop oldWorld (newTime - oldTime)
--				return newWorld

-- | Combined update from time and messages
updateWorld :: ([Message] -> world -> world)           -- ^ A function to handle messages from clients.
            -> (Double -> world -> (world, [Message])) -- ^ A function to step the world one iteration, given the past time
            -> (world -> [Message] -> Double -> (world, [Message]))

updateWorld messageUpdate timeUpdate world messages time = timeUpdate time (messageUpdate messages world)

-- | Run the update with the requisite communication over the network
--network :: (world -> [Message] -> Double -> (world, [Message])) -> (inbox, outbox) -> world -> Double  -> IO world
--network calculate (inbox, outbox) world time = do
--	inMessages <- read inbox
--	(newWorld, outMessages) <- return $ calculate world inMessages time
--	append outbox outMessages
--	return newWorld


{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Serenity.Game.Server.Main
(	server
,	connectionPhase
,	sendToClients
,	getCommands
) where

import Serenity.Game.Server.ClientData
import Serenity.Model
import Serenity.Model.Wire
import Serenity.Network.Transport
import Serenity.Network.Utility

import Prelude hiding (id, (.))

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Run the server.
server 
	:: Int   -- ^ Port to listen on.
	-> Int   -- ^ Number of clients to connect.
	-> IO ()
server port clientCount = do
	print "server started"	
	print $ "waiting for " ++ (show clientCount) ++ " clients to connect..."
	clients <- connectionPhase (fromIntegral port) clientCount
	print "all clients connected, starting game"
	play 5 clients demoGame commands evolve updates
	print "server finished"

-- | Wait for n clients to connect
connectionPhase
	:: PortNumber      -- ^ Port to listen on.
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
			game'             <- return $ updateWorld updatesC game
			newTime           <- getCurrentTime
			time              <- return $ toRational $ diffUTCTime newTime lastTime
			(updatesT, wire') <- return $ runWire wire (fromRational time) (game', game')
			game''            <- return $ gameTime +~ (fromRational time) $ updateWorld updatesT game'
			game'''           <- return $ gameRandom %~ (snd.next) $ game''
			sendToClients (updatesC ++ updatesT) clientDataList
			threadDelay $ floor (1000000 / (fromIntegral stepsPerSecond))
			playLoop (game''', wire') newTime

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




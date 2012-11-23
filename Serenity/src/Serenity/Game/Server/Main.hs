{-# LANGUAGE ExplicitForAll #-}

module Serenity.Game.Server.Main
(	main
,	port
,	updateWorld
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport
import Serenity.Network.Packet
import Serenity.Network.Message

port = 9900

main = print "sup"

-- main = do
-- 	connection <- runListen port
-- 	return ()

readInput sock tvar = do
	(packet, client) <- receivePacket sock
	atomically $ writeTVar tvar (packetData packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)

-- | Run the server with given update functions.
--play :: forall world
--	 .  Int                            -- ^ Number of simulation steps to take for each second of real time.
--	 -> PortNumber                     -- ^ Port to listen on
--	 -> world                          -- ^ The initial world.
--	 -> ([Message] -> world -> world)  -- ^ A function to handle messages from clients.
--	 -> (Double -> world -> [Message]) -- ^ A function to step the world one iteration, given the past time
--	 -> IO ()

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


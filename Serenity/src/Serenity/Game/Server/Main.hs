module Serenity.Game.Server.Main
(	main
,	port
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport
import Serenity.Network.Packet

port = 9900

main = do
	connection <- runListen port
	return ()

readInput sock tvar = do
	(packet, client) <- receivePacket sock
	atomically $ writeTVar tvar (packetData packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)
module Serenity.Game.Server.Main (
	main,
        port
) 
where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Network.Socket hiding (send, sendTo, recv, recvFrom, listen)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import System.Posix.IO

import Serenity.Network.Transport
import Serenity.Network.Packet

port = 9900

main = do
	connection <- run_listen port
	return ()

read_input sock tvar = do
	(packet, client) <- receive_packet sock
	atomically $ writeTVar tvar (packet_data packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)
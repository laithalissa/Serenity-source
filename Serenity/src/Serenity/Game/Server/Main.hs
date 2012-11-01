module Serenity.Game.Server.Main (
	main
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

main = withSocketsDo $ do 
	putStrLn "---Serenity Server V0.1---"
	putStrLn $ "Accepting connections on port " ++ (show port) ++ "..."

	connection <- run_listen port
	return ()

	--sock <- socket AF_INET Datagram 0
	--bindSocket sock (SockAddrInet port iNADDR_ANY)
	--input_var <- newTVarIO (C.pack "start")
	--forever $ read_input sock input_var

read_input sock tvar = do
	(packet, client) <- receive_packet sock
	atomically $ writeTVar tvar (packet_data packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)
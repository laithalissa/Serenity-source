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

import Serenity.Network
import qualified Serenity.Game.Server.GameSupervisor as GameSupervisor


port = 9900

main = do
  GameSupervisor.start

-- main = withSocketsDo $ do 
-- 	putStrLn "---Serenity Server V0.1---"
-- 	putStrLn $ "Accepting connections on port " ++ (show port) ++ "..."

-- 	connection <- listen port
-- 	return ()

	--sock <- socket AF_INET Datagram 0
	--bindSocket sock (SockAddrInet port iNADDR_ANY)
	--input_var <- newTVarIO (C.pack "start")
	--forever $ read_input sock input_var

read_input sock tvar = do
	(packet, client) <- receive sock
	atomically $ writeTVar tvar (packet_data packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)
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

import Serenity.Network

port = 9900

main = withSocketsDo $ do 
	putStrLn "---Serenity Server V0.1---"
	putStrLn $ "Accepting connections on port " ++ (show port) ++ "..."

	connection <- listen port
	return ()

	--input_var <- newTVarIO (C.pack "start")
	--forever $ read_input sock input_var

read_input sock tvar = do
	(packet, client) <- receive sock
	atomically $ writeTVar tvar (p_data packet)
	putStrLn $ "[" ++ (show client) ++ "] " ++ (show packet)
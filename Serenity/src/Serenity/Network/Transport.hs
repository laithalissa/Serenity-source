{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serenity.Network.Transport (
	Connection (..)
,	Packet (..)
,	run_transport
,	eval_transport
,	run_connect
,	run_listen
,	connect
,	listen
,	send
,	receive
,	is_connected
,	get_connection
,	liftIO
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect)
import Network.Socket.ByteString hiding (send)
import System.Posix.IO
import Data.Time.Clock.POSIX

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (liftM)
import Control.Monad.State
import Data.Monoid

import Serenity.Network.Packet (Packet(..), initial_packet, receive_packet, send_packet, get_packet_data)

data Connection = 
	Connected 
	{	connection_socket :: Socket
	,	connection_addr :: SockAddr
	,	connection_id :: Int
	,	connection_sent :: Set (Packet, POSIXTime)
	,	connection_local_sequence ::  Int
	,	connection_remote_sequence ::  Int
	} 
	| Unconnected deriving (Show, Eq)

initial_connection sock addr cid = Connected 
	{	connection_socket = sock
	,	connection_addr = addr
	,	connection_id = cid
	,	connection_sent = Set.empty
	,	connection_local_sequence = 1
	,	connection_remote_sequence = 1
	}

is_connected :: Connection -> Bool
is_connected Connected {} = True
is_connected _ = False

bind_outbound_socket host port = withSocketsDo $ do
	addr_info <- 
		liftM head $ 
		liftM (filter (\x -> addrFamily x == AF_INET)) $ 
		getAddrInfo Nothing (Just "localhost") (Just (show port))
	let addr = addrAddress addr_info
	let family = addrFamily addr_info

	sock <- socket family Datagram 0
	bindSocket sock (SockAddrInet (port+1) iNADDR_ANY)
	return (sock, addr, family)

class Monad m => MonadTransport m where
	connect :: String -> PortNumber -> m ()
	listen  :: PortNumber -> m ()
	send :: String -> m ()
	receive :: m String
	get_connection :: m Connection

newtype Transport a = Transport (StateT Connection IO a)
	deriving (Functor, Monad, MonadIO, MonadState Connection)

instance MonadTransport (Transport) where
	connect host port = do 
		connection <- get
		case connection of
			Connected {} -> return ()
			Unconnected -> do
				(sock, addr, family) <- liftIO $ bind_outbound_socket host port
				put $ initial_connection sock addr 13
				liftIO $ send_packet sock (initial_packet "HELLO") addr
				return ()

	listen port = do 
		connection <- liftIO (withSocketsDo $ do
			sock <- socket AF_INET Datagram 0
			bindSocket sock (SockAddrInet port iNADDR_ANY)

			(packet, client) <- receive_packet sock 
			if (get_packet_data packet) == "HELLO"
				then return $ initial_connection sock client 12
				else return Unconnected)
		put connection

	send string = do
		connection <- get
		liftIO $ send_packet 
			(connection_socket connection) 
			(initial_packet string) 
			(connection_addr connection)

	receive = do
		connection <- get
		(packet, client) <- liftIO $ receive_packet (connection_socket connection)
		return $ get_packet_data packet

	get_connection = get

instance (Monoid a) => Monoid (Transport a) where
	mempty = return mempty
	m1 `mappend` m2 = do
		x1 <- m1
		x2 <- m2
		return (x1 `mappend` x2)

eval_transport :: Transport a -> Connection -> IO a
eval_transport (Transport c) =  evalStateT c

run_transport :: Transport a -> Connection -> IO (a, Connection)
run_transport (Transport c) =  runStateT c

run_one :: Transport a -> IO Connection
run_one f = do 
	(_, connection) <- run_transport f Unconnected
	return connection

run_connect host port = run_one $ connect host port
run_listen port = run_one $ listen port

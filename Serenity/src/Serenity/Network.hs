{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serenity.Network (
	Connection (..)
,	Packet (..)
,	listen
,	send_packet
,	receive_packet
,	connect
,	run_transport
,	eval_transport
,	run_connect
,	run_listen
,	connect_
,	listen_
,	send_
,	receive_
,	is_connected
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect)
import Network.Socket.ByteString hiding (send)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import System.Posix.IO
import Data.Time.Clock.POSIX

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (liftM)
import Control.Monad.State
import Data.Monoid

import Serenity.Network.Packet

data Connection = 
	Connected 
	{	connection_socket :: Socket
	,	connection_addr :: SockAddr
	,	connection_id :: Int
	,	connection_sent :: Set (Packet, POSIXTime)
	,	connection_local_sequence ::  Int
	,	connection_remote_sequence ::  Int
	} 
	| Unconnected

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

listen port = withSocketsDo $ do
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet port iNADDR_ANY)

	(packet, client) <- receive_packet sock 
	if (get_packet_data packet) == "HELLO"
		then return $ initial_connection sock client 12
		else return Unconnected

connect port = withSocketsDo $ do
	addr_info <- liftM head $ liftM (filter (\x -> addrFamily x == AF_INET)) $ 
		getAddrInfo Nothing (Just "localhost") (Just (show port))
	let addr = addrAddress addr_info
	let family = addrFamily addr_info

	sock <- socket family Datagram 0
	bindSocket sock (SockAddrInet (port+1) iNADDR_ANY)

	send_packet sock (initial_packet "HELLO") addr

bind_outbound_socket port host = withSocketsDo $ do
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
	connect_ :: PortNumber -> m ()
	listen_  :: PortNumber -> m ()
	send_ :: String -> m ()
	receive_ :: m String

newtype Transport a = Transport (StateT Connection IO a)
	deriving (Functor, Monad, MonadIO, MonadState Connection)

instance MonadTransport (Transport) where
	connect_ port = do 
		connection <- get
		case connection of
			Connected {} -> return ()
			Unconnected -> do
				(sock, addr, family) <- liftIO $ bind_outbound_socket port "localhost"
				put $ initial_connection sock addr 13
				liftIO $ send_packet sock (initial_packet "HELLO") addr
				return ()

	listen_ port = do 
		connection <- liftIO (listen port)
		put connection

	send_ string = do
		connection <- get
		liftIO $ send_packet 
			(connection_socket connection) 
			(initial_packet string) 
			(connection_addr connection)

	receive_ = do
		connection <- get
		(packet, client) <- liftIO $ receive_packet (connection_socket connection)
		return $ get_packet_data packet

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

run_connect port = run_one (connect_ port)
run_listen port = run_one (listen_ port)

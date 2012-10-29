module Serenity.Network (
	Connection (..)
,	Packet (..)
,	listen
,	send_packet
,	receive_packet
,	connect
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

receive_packet sock = do
	(mesg, client) <- recvFrom sock 512
	maybe_packet <- return $ read_packet mesg
	case maybe_packet of 
		Just packet -> return (packet, client)
		Nothing -> receive_packet sock

send_packet sock packet = do
	sendAllTo sock (write_packet packet)

listen port = withSocketsDo $ do
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet port iNADDR_ANY)

	(packet, client) <- receive_packet sock 
	if (packet_data packet) == C.pack "HELLO"
		then accept sock client
		else reject sock client

accept sock client = do
	return $ initial_connection sock client 12

reject sock client = do
	return Unconnected

connect port = withSocketsDo $ do
	addr_info <- liftM head $ liftM (filter (\x -> addrFamily x == AF_INET)) $ 
		getAddrInfo Nothing (Just "localhost") (Just (show port))
	let addr = addrAddress addr_info
	let family = addrFamily addr_info

	sock <- socket family Datagram 0
	bindSocket sock (SockAddrInet (port+1) iNADDR_ANY)

	send_packet sock (initial_packet "HELLO") addr


module Serenity.Network (
	Packet(..)
,	receive
,	send
,	listen
,	connect
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect)
import Network.Socket.ByteString hiding (send)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Word (Word32)
import qualified Data.Word as Word
import System.Posix.IO
--import Data.Time.Clock.POSIX

import Text.Printf
import Control.Monad (liftM)
import Control.Concurrent.STM

data Packet = Packet
	{	p_prot :: Word32
	,	p_seq  :: Word32
	,	p_ack  :: Word32
	,	p_data :: C.ByteString
	} deriving Show

read_packet :: ByteString -> Packet
read_packet bytes = Packet
	{	p_prot = fromIntegral $ fst read_prot
	,	p_seq  = fromIntegral $ fst read_seq
	,	p_ack  = fromIntegral $ fst read_ack
	,	p_data = dat
	} where
		(prot_bytes, after_seq) = C.splitAt 32 bytes
		(seq_bytes, after_prot) = C.splitAt 32 after_seq
		(ack_bytes, dat) = C.splitAt 32 after_prot
		read_int input = maybe (fromIntegral 0, C.empty) id (C.readInt input)
		read_prot = read_int prot_bytes
		read_seq  = read_int seq_bytes
		read_ack  = read_int ack_bytes

write_packet :: Packet -> ByteString
write_packet packet = C.concat [prot, sequ, ack, p_data packet] where
	prot = pad32 $ p_prot packet
	sequ = pad32 $ p_seq  packet
	ack  = pad32 $ p_ack  packet

pad32 = C.pack . (printf "%032i")

initial_packet string = Packet 
		{	p_prot = fromIntegral $ 1
		,	p_seq  = fromIntegral $ 1
		,	p_ack  = fromIntegral $ 1
		,	p_data = C.pack string
		}

data Connection = 
	Connected 
	{	connection_socket :: Socket
	,	connection_addr :: SockAddr
	,	connection_id :: Int
	--,	connection_sent :: TVar (Packet, POSIXTime)
	,	connection_local_sequence ::  Int
	,	connection_remote_sequence ::  Int
	} 
	| Unconnected

initial_connection sock addr cid = Connected 
	{	connection_socket = sock
	,	connection_addr = addr
	,	connection_id = cid
	,	connection_local_sequence = 1
	,	connection_remote_sequence = 1
	}

receive sock = do
	(mesg, client) <- recvFrom sock 512
	packet <- return $ read_packet mesg
	return (packet, client)

send sock packet = do
	sendAllTo sock (write_packet packet)

listen port = withSocketsDo $ do
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet port iNADDR_ANY)

	(packet, client) <- receive sock 
	if (p_data packet) == C.pack "HELLO"
		then accept sock client
		else reject sock client

accept sock client = do
	print "accept"
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

	send sock (initial_packet "HELLO") addr


module Serenity.Network.Packet (
	Packet (..)
,	initial_packet
,	read_packet
,	write_packet
,	get_packet_data
,	receive_packet
,	send_packet
)
where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect)
import Network.Socket.ByteString hiding (send)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Word (Word32)
import qualified Data.Word as Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Strict.Get

data Packet = Packet
	{	packet_prot :: Word32
	,	packet_seq  :: Word32
	,	packet_ack  :: Word32
	,	packet_data :: C.ByteString
	} deriving (Show, Eq)

read_packet :: ByteString -> Maybe Packet
read_packet bytes = case (fst $ read_packet' bytes) of 
	Left _ -> Nothing
	Right packet -> Just packet
read_packet' = runGet $ do
	p_prot <- getWord32be; p_seq  <- getWord32be; p_ack  <- getWord32be; 
	r <- remaining; p_data <- getByteString r
	return Packet {packet_prot = p_prot, packet_seq = p_seq, packet_ack = p_ack, packet_data = p_data}

write_packet :: Packet -> ByteString
write_packet packet = B.concat $ BL.toChunks $ runPut $ do
	putWord32be $ packet_prot packet
	putWord32be $ packet_seq packet
	putWord32be $ packet_ack packet
	putByteString $ packet_data packet

initial_packet string = Packet 
	{	packet_prot = fromIntegral $ 1
	,	packet_seq  = fromIntegral $ 1
	,	packet_ack  = fromIntegral $ 1
	,	packet_data = C.pack string
	}

get_packet_data :: Packet -> String
get_packet_data Packet {packet_data = d} = C.unpack d

receive_packet sock = do
	(mesg, client) <- recvFrom sock 512
	maybe_packet <- return $ read_packet mesg
	case maybe_packet of 
		Just packet -> return (packet, client)
		Nothing -> receive_packet sock

send_packet sock packet = do
	sendAllTo sock (write_packet packet)
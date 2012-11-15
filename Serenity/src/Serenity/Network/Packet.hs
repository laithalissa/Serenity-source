module Serenity.Network.Packet (
	Packet (..)
,	initial_packet
,	read_packet
,	write_packet
,	get_packet_data
,	receive_packet
,	send_packet
,	Flag (..)
,	emptySynPacket
,	emptyFinPacket
,	setFlags
,	getFlags
)
where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect)
import Network.Socket.ByteString hiding (send)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Word (Word32, Word8)
import qualified Data.Word as Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Strict.Get

import Serenity.Network.Message
import Data.Binary (encode, decode)
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as Map

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

data Packet = Packet
	{	packet_prot  :: Word32
	,	packet_seq   :: Word32
	,	packet_ack   :: Word32
	,	packet_flags :: Word8
	,	packet_data  :: C.ByteString
	} deriving (Show, Eq)

read_packet :: ByteString -> Maybe Packet
read_packet bytes = case (fst $ read_packet' bytes) of 
	Left _ -> Nothing
	Right packet -> Just packet
read_packet' = runGet $ do
	p_prot  <- getWord32be 
	p_seq   <- getWord32be
	p_ack   <- getWord32be
	p_flags <- getWord8
	r       <- remaining
	p_data  <- getByteString r
	return Packet 
		{	packet_prot = p_prot
		,	packet_seq = p_seq
		,	packet_ack = p_ack
		,	packet_flags = p_flags
		,	packet_data = p_data
		}

messageToBinary :: Message -> ByteString
messageToBinary message = B.concat $ BL.toChunks $ encode message

binaryToMessage :: ByteString -> Message
binaryToMessage message = decode (BL.fromChunks [message])

write_packet :: Packet -> ByteString
write_packet packet = B.concat $ BL.toChunks $ runPut $ do
	putWord32be   $ packet_prot packet
	putWord32be   $ packet_seq packet
	putWord32be   $ packet_ack packet
	putWord8      $ packet_flags packet
	putByteString $ packet_data packet

initial_packet :: Message -> Packet
initial_packet message = Packet 
	{	packet_prot  = fromIntegral $ 1
	,	packet_seq   = fromIntegral $ 1
	,	packet_ack   = fromIntegral $ 1
	,	packet_flags = fromIntegral $ 0
	,	packet_data  = messageToBinary message
	}

get_packet_data :: Packet -> Message
get_packet_data Packet {packet_data = dat} = binaryToMessage dat

data Flag = Syn | Fin deriving (Eq, Ord, Show)

flagValues :: [(Flag, Word8)]
flagValues = 
	[	(Syn, 0)
	,	(Fin, 1)
	]

getFlags :: Packet -> [Flag]
getFlags packet@Packet{packet_flags=flagsField} = 
	concat $ zipWith flagInField (map snd flagValues) (map fst flagValues)
	where
		flagInField i flag = if (bit $ fromIntegral i) .&. flagsField == (bit $ fromIntegral i) then [flag] else []

setFlags :: [Flag] -> Packet -> Packet
setFlags [] packet = packet {packet_flags = 0}
setFlags flags packet = packet {packet_flags = field} where
	field = foldl buildField 0 flags
	buildField currentBits flag = bit (fromIntegral ((Map.fromList flagValues) Map.! flag)) .|. currentBits

emptySynPacket = setFlags [Syn] (initial_packet Message.Empty)
emptyFinPacket = setFlags [Fin] (initial_packet Message.Empty)

receive_packet sock = do
	(mesg, client) <- recvFrom sock 512
	maybe_packet <- return $ read_packet mesg
	case maybe_packet of 
		Just packet -> return (packet, client)
		Nothing -> receive_packet sock

send_packet sock packet = do
	sendAllTo sock (write_packet packet)



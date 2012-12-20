module Serenity.Network.Packet
(	Packet (..)
,	initialPacket
,	readPacket
,	writePacket
,	getPacketData
,	receivePacket
,	sendPacket
,	Flag (..)
,	emptySynPacket
,	emptySynAckPacket
,	emptyFinPacket
,	setFlags
,	getFlags
,	messageToBinary
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
	{	packetProt :: Word32
	,	packetSeq :: Word32
	,	packetAck :: Word32
	,	packetAckBits :: Word32
	,	packetFlags :: Word8
	,	packetData :: C.ByteString
	} deriving (Show, Eq)

readPacket :: ByteString -> Maybe Packet
readPacket bytes = case (fst $ readPacket' bytes) of
	Left _ -> Nothing
	Right packet -> Just packet
readPacket' = runGet $ do
	pProt  <- getWord32be
	pSeq   <- getWord32be
	pAck   <- getWord32be
	pAckBits <- getWord32be
	pFlags <- getWord8
	r       <- remaining
	pData  <- getByteString r
	return Packet
		{	packetProt = pProt
		,	packetSeq = pSeq
		,	packetAck = pAck
		,	packetAckBits = pAckBits
		,	packetFlags = pFlags
		,	packetData = pData
		}

messageToBinary :: Message -> ByteString
messageToBinary message = B.concat $ BL.toChunks $ encode message

binaryToMessage :: ByteString -> Message
binaryToMessage message = decode (BL.fromChunks [message])

writePacket :: Packet -> ByteString
writePacket packet = B.concat $ BL.toChunks $ runPut $ do
	putWord32be   $ packetProt packet
	putWord32be   $ packetSeq packet
	putWord32be   $ packetAck packet
	putWord32be   $ packetAckBits packet
	putWord8      $ packetFlags packet
	putByteString $ packetData packet

initialPacket :: Message -> Packet
initialPacket message = Packet
	{	packetProt = fromIntegral 1
	,	packetSeq = fromIntegral 1
	,	packetAck = fromIntegral 1
	,	packetAckBits = fromIntegral 0
	,	packetFlags = fromIntegral 0
	,	packetData = messageToBinary message
	}

getPacketData :: Packet -> Message
getPacketData Packet {packetData = dat} = binaryToMessage dat

data Flag = Syn | Ack | Fin deriving (Eq, Ord, Show)

flagValues :: [(Flag, Word8)]
flagValues =
	[	(Syn, 0)
	,	(Fin, 1)
	,	(Ack, 2)
	]

getFlags :: Packet -> [Flag]
getFlags packet@Packet{packetFlags=flagsField} =
	concat $ zipWith flagInField (map snd flagValues) (map fst flagValues)
	where
		flagInField i flag = if (bit $ fromIntegral i) .&. flagsField == (bit $ fromIntegral i) then [flag] else []

setFlags :: [Flag] -> Packet -> Packet
setFlags [] packet = packet {packetFlags = 0}
setFlags flags packet = packet {packetFlags = field} where
	field = foldl buildField 0 flags
	buildField currentBits flag = bit (fromIntegral ((Map.fromList flagValues) Map.! flag)) .|. currentBits

emptySynPacket = setFlags [Syn] (initialPacket Message.Empty)
emptySynAckPacket = setFlags [Syn, Ack] (initialPacket Message.Empty)
emptyFinPacket = setFlags [Fin] (initialPacket Message.Empty)

receivePacket sock = do
	(mesg, client) <- recvFrom sock 512
	maybePacket <- return $ readPacket mesg
	case maybePacket of
		Just packet -> return (packet, client)
		Nothing -> receivePacket sock

sendPacket sock packet = do
	sendAllTo sock (writePacket packet)


module Serenity.Network.Connection where

import Data.Bits
import Data.Word (Word32)

import Serenity.Network.Message
import Serenity.Network.Packet

data Connection = Connection
	{	connectionState :: ConnectionState
	,	connectionReliability :: Reliability
	}
	deriving (Show, Eq)

data ConnectionState = Connecting
	| Connected
	| Disconnected
	| TimedOut
	deriving (Show, Eq)

data Reliability = Reliability
	{	localSequence :: Word32 -- ^ Sequence number of most recently sent packet
	,	remoteSequence :: Word32 -- ^ Sequence number of most recently received packet
	,	pendingAckPackets :: [Word32] -- ^ Sent packets that haven't been acked yet
	,	receivedPackets :: [Word32] -- ^ Received packets to be acked
	}
	deriving (Show, Eq)

initialConnection :: Connection
initialConnection = Connection
	{	connectionState = Connecting
	,	connectionReliability = initReliability
	}

isConnected :: Connection -> Bool
isConnected Connection {connectionState = Connected} = True
isConnected _ = False

connectionPacket :: Connection -> Message -> Packet
connectionPacket (Connection {connectionReliability = r}) message = Packet
	{	packetProt = fromIntegral 1
	,	packetSeq = fromIntegral $ localSequence r
	,	packetAck = fromIntegral $ remoteSequence r
	,	packetAckBits = generateAckBits r
	,	packetFlags = fromIntegral 0
	,	packetData = messageToBinary message
	}

-- | Create initial reliability system
initReliability :: Reliability
initReliability = Reliability
	{	localSequence = 0
	,	remoteSequence = 0
	,	pendingAckPackets = []
	,	receivedPackets = []
	}

-- | Update reliability system after some time has passed
updateReliability :: Float -> Reliability -> Reliability
updateReliability delta r = r

-- | Update reliability system after a packet has been sent
packetSent :: Reliability -> Reliability
packetSent r = r { localSequence = (localSequence r) + 1 }

-- | Update reliability system after a packet was received
packetReceived :: Word32 -> Reliability -> Reliability
packetReceived seq r@(Reliability {receivedPackets = rec, remoteSequence = rem}) = r
	{ receivedPackets = updateReceived rec seq
	, remoteSequence = seq'
	}
	where
		updateReceived received seq = filter (\s -> s `moreRecentThan` min) (seq:received)
		seq' = if seq `moreRecentThan` rem then seq else rem
		min = if seq' >= 34 then seq' - 34 else maxSequence - (34 - seq')

generateAckBits :: Reliability -> Word32
generateAckBits (Reliability {receivedPackets = packets, remoteSequence = seq}) = foldl setAckBit 0 packets
	where
		setAckBit ackBits packet = if packet /= seq then ackBits `setBit` (fromIntegral $ getBit packet) else ackBits
		getBit packet = if packet > seq
											then seq + (maxSequence - packet)
											else seq - 1 - packet

moreRecentThan :: Word32 -> Word32 -> Bool
x `moreRecentThan` y = x > y && (x - y <= 1000) || x < y && (y - x > maxSequence - 1000)

maxSequence :: Word32
maxSequence = maxBound

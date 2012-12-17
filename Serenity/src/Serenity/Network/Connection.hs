module Serenity.Network.Connection where

import Data.Word (Word32)

data Connection = Connection
	{	connectionState :: ConnectionState
	,	connectionReliability :: Reliability
	}

data ConnectionState = Connecting
	| Connected
	| Disconnected
	| TimedOut

data Reliability = Reliability
	{	localSequence :: Int -- ^ Sequence number of most recently sent packet
	,	remoteSequence :: Int -- ^ Sequence number of most recently received packet
	,	pendingAckPackets :: [Int] -- ^ Sent packets that haven't been acked yet
	, receivedPackets :: [Int] -- ^ Received packets to be acked
	}

initialConnection :: Connection
initialConnection = Connection
	{	connectionState = Connecting
	,	connectionReliability = initReliability
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
packetSent r = r

-- | Update reliability system after a packet was received
packetReceived :: Word32 -> Reliability -> Reliability
packetReceived seq r = r

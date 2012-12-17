module Serenity.Network.Transport
(	TransportInterface(..)
,	initTransport
,	acceptClient
,	connect
,	sendAndReceive
,	receive
,	send
,	PortNumber
)
where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State
import qualified Data.Map as M
import Network.Socket hiding (connect, send)

import Serenity.Network.Connection
import Serenity.Network.Message (Message)
import Serenity.Network.Packet

-- | The network transport represents a list of connected
-- clients and a socket that can be used to communicate
-- with them.
type Transport = (ConnectionMap, Socket)

-- | Mapping from client address to a connection and its
-- related inbox/outbox channels.
type ConnectionMap = M.Map SockAddr TransportInterface

-- | Connection information and related Inbox/outbox channels
-- used to communicate with another machine.
data TransportInterface = TransportInterface
	{	channelInbox  :: TChan Message
	,	channelOutbox :: TChan Message
	, channelConnection :: TVar Connection
	}

-- | Create the initial server transport.
-- A socket bound to the given port and an empty client map.
initTransport :: PortNumber -> IO Transport
initTransport port = withSocketsDo $ do
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet port iNADDR_ANY)
	return (M.empty, sock)

getOutboundSocket :: HostName -> PortNumber -> IO (Socket, SockAddr)
getOutboundSocket host port = do
	addrInfo <- liftM head $
		liftM (filter (\x -> addrFamily x == AF_INET)) $
		getAddrInfo Nothing (Just host) (Just (show port))
	let addr = addrAddress addrInfo
	let family = addrFamily addrInfo

	sock <- socket family Datagram 0
	bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
	return (sock, addr)

connect :: HostName -> PortNumber -> IO Transport
connect host port = do
	(sock, addr) <- getOutboundSocket host port
	channels <- newTransportInterface
	sendPacket sock emptySynPacket addr
	return (M.singleton addr channels, sock)

-- | Computation that accepts a connection from a new client and provides
-- inbox/outbox channels to communicate with it.
--
-- It might be simpler to have explicit state i.e.
-- acceptClient :: ServerTransport -> IO (TransportInterface, ServerTransport)
acceptClient :: StateT Transport IO TransportInterface
acceptClient = do
	(clients, sock) <- get
	client <- lift $ acceptClient' (clients, sock)
	channels <- lift newTransportInterface
	put $ (M.insert client channels clients, sock)
	return channels

acceptClient' :: Transport -> IO SockAddr
acceptClient' (clients, sock) = do
	(packet, client) <- receivePacket sock
	if Syn `elem` (getFlags packet) && M.notMember client clients
		then return client
		else acceptClient' (clients, sock)

-- | Start communications with clients connected to the given
-- server transport.
sendAndReceive :: Transport -> IO ()
sendAndReceive (clients, sock) = do
	socketTVar <- atomically $ newTVar sock
	forkIO $ forever $ inboxLoop clients socketTVar
	forkIO $ forever $ outboxLoop clients socketTVar
	return ()
	where
		inboxLoop clients socketTVar = do
			sock <- atomically $ readTVar socketTVar
			maybe <- receive clients sock
			case maybe of
				Just (message, channels) -> atomically $ writeTChan (channelInbox channels) message
				Nothing -> return ()

		outboxLoop clients socketTVar = do
			sock <- atomically $ readTVar socketTVar
			mapM_ (readAndSend sock) (M.toList clients)
			threadDelay 1000
			return ()

		readAndSend sock (addr, channels) = do
			message <- atomically $ tryReadTChan (channelOutbox channels)
			case message of
				Just m -> send (channelConnection channels) sock m addr
				Nothing -> return ()

-- | Get the next Message along with the address of the
-- sender from the given socket.
receive :: ConnectionMap -> Socket -> IO (Maybe (Message, TransportInterface))
receive clients sock = do
	(packet, addr) <- receivePacket sock
	case M.lookup addr clients of
		Just channels -> do
			-- TODO Packet verification
			updateChannels channels packet
			return $ Just (getPacketData packet, channels)
		Nothing -> return Nothing
	where
		updateChannels (TransportInterface {channelConnection = connTVar}) packet = do
			-- TODO Use modifyTVar
			conn <- atomically $ readTVar connTVar
			atomically $ writeTVar connTVar (conn { connectionReliability = packetReceived (packetSeq packet) (connectionReliability conn) })

-- | Send a Message on the given socket to the specified address.
send :: TVar Connection -> Socket -> Message -> SockAddr -> IO ()
send connTVar sock message addr = do
	-- TODO Get sequence ID and acks from reliability
	sendPacket sock (initialPacket message) addr
	-- TODO Use modifyTVar
	conn <- atomically $ readTVar connTVar
	atomically $ writeTVar connTVar (conn { connectionReliability = packetSent (connectionReliability conn) })

newTransportInterface :: IO TransportInterface
newTransportInterface = do
	inbox <- atomically $ newTChan
	outbox <- atomically $ newTChan
	connection <- atomically $ newTVar initialConnection
	return $ TransportInterface
		{	channelInbox = inbox
		,	channelOutbox = outbox
		,	channelConnection = connection
		}

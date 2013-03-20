module Serenity.Network.Transport
(	Transport(..)
,	TransportInterface(..)
,	listen
,	acceptClient
,	connect
,	connectTo
,	closeTransport
,	sendAndReceive
,	receive
,	send
,	newTransportInterface
,	PortNumber
)
where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State
import qualified Data.Map as M
import Network.Socket hiding (Connected, connect, listen, send)

import Serenity.Model.Message
import Serenity.Network.Connection
import Serenity.Network.Packet

-- | The network transport represents a list of connected
-- clients and a socket that can be used to communicate
-- with them.
data Transport = Transport
	{	transportConnectionMap :: ConnectionMap
	,	transportSocket :: Socket
	,	transportEntry :: TChan (Packet, SockAddr)
	}

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
listen :: PortNumber -> IO Transport
listen port = withSocketsDo $ do
	sock <- socket AF_INET Datagram 0
	bindSocket sock (SockAddrInet port iNADDR_ANY)
	chan <- atomically $ newTChan
	return $ Transport
		{	transportConnectionMap = M.empty
		,	transportSocket = sock
		,	transportEntry = chan
		}

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
	send (channelConnection channels) sock Empty addr
	chan <- atomically $ newTChan
	return $ Transport
		{	transportConnectionMap = M.singleton addr channels
		,	transportSocket = sock
		,	transportEntry = chan
		}

connectTo :: HostName -> PortNumber -> IO TransportInterface
connectTo host port = do
	transport <- connect host port
	transportVar <- atomically $ newTVar transport
	sendAndReceive transportVar
	let [channels] = M.elems (transportConnectionMap transport)
	return channels

-- | Computation that accepts a connection from a new client and provides
-- inbox/outbox channels to communicate with it.
acceptClient :: TVar Transport -> IO TransportInterface
acceptClient transportVar = do
	transport <- atomically $ readTVar transportVar
	(client, channels) <- acceptClient' transport
	let clients' = M.insert client channels (transportConnectionMap transport)
	atomically $ writeTVar transportVar (transport {transportConnectionMap = clients'})
	return channels

acceptClient' :: Transport -> IO (SockAddr, TransportInterface)
acceptClient' transport@Transport{transportConnectionMap = clients, transportEntry = entry} = do
	(packet, client) <- atomically $ readTChan entry
	if Syn `elem` (getFlags packet) && M.notMember client clients
		then do
			channels <- newTransportInterface
			updateConnection (channelConnection channels) (connectionReceivedPacket packet)
			-- send (channelConnection channels) sock Empty client
			atomically $ writeTChan (channelOutbox channels) Empty
			return (client, channels)
		else acceptClient' transport

closeTransport :: Transport -> IO ()
closeTransport transport = close (transportSocket transport)

-- | Start communications with clients connected to the given
-- server transport.
sendAndReceive :: TVar Transport -> IO ()
sendAndReceive transportVar = do
	forkIO $ forever $ inboxLoop transportVar
	forkIO $ forever $ outboxLoop transportVar
	return ()
	where
		inboxLoop transportVar = do
			maybe <- receive transportVar
			case maybe of
				Just (message, channels) -> atomically $ writeTChan (channelInbox channels) message
				Nothing -> return ()

		outboxLoop transportVar = do
			transport <- atomically $ readTVar transportVar
			mapM_ (readAndSend $ transportSocket transport) (M.toList $ transportConnectionMap transport)
			threadDelay 1000
			return ()

		readAndSend sock (addr, channels) = do
			message <- atomically $ tryReadTChan (channelOutbox channels)
			case message of
				Just m -> send (channelConnection channels) sock m addr
				Nothing -> return ()

-- | Get the next Message along with the address of the
-- sender from the given socket.
receive :: TVar Transport -> IO (Maybe (Message, TransportInterface))
receive transportVar = do
	Transport{transportSocket = sock} <- atomically $ readTVar transportVar
	(packet, addr) <- receivePacket sock
	Transport{transportConnectionMap = clients, transportSocket = sock, transportEntry = entry} <- atomically $ readTVar transportVar
	case M.lookup addr clients of
		Just channels -> do
			-- TODO Packet verification
			updateConnection (channelConnection channels) (connectionReceivedPacket packet)
			return $ Just (getPacketData packet, channels)
		Nothing -> do
			atomically $ writeTChan entry (packet, addr)
			return Nothing

-- | Send a Message on the given socket to the specified address.
send :: TVar Connection -> Socket -> Message -> SockAddr -> IO ()
send connTVar sock message addr = do
	conn <- atomically $ readTVar connTVar
	let packet = connectionPacket conn message
	sendPacket sock packet addr
	updateConnection connTVar connectionSentPacket

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

updateConnection :: TVar Connection -> (Connection -> Connection) -> IO ()
updateConnection tvar f = atomically $ modifyTVar' tvar f

connectionReceivedPacket :: Packet -> Connection -> Connection
connectionReceivedPacket packet connection = connection
	{	connectionReliability = packetReceived (packetSeq packet) (connectionReliability connection)
	,	connectionState = Connected
	}

connectionSentPacket :: Connection -> Connection
connectionSentPacket connection = connection
	{	connectionReliability = packetSent (connectionReliability connection)
	}

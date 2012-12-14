module Serenity.Network.Server
(	TransportInterface(..)
,	initTransport
, acceptClient
, serveClients
, PortNumber
)
where

import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State
import qualified Data.Map as M
import Network.Socket

import Serenity.Network.Message (Message)
import Serenity.Network.Packet

-- | A server transport represents a list of connected
-- clients and a socket that can be used to communicate
-- with them.
type ServerTransport = (ClientMap, Socket)

-- | Mapping from client address to inbox/outbox channels.
type ClientMap = M.Map SockAddr TransportInterface

-- | Inbox/outbox channels used to communicate with a client.
data TransportInterface = TransportInterface
	{	channelInbox  :: TChan Message
	,	channelOutbox :: TChan Message
	}

-- | Create the initial server transport.
-- A socket bound to the given port and an empty client map.
initTransport :: PortNumber -> IO ServerTransport
initTransport port = withSocketsDo $ do
			sock <- socket AF_INET Datagram 0
			bindSocket sock (SockAddrInet port iNADDR_ANY)	
			return (M.empty, sock)

-- | Computation that accepts a connection from a new client and provides
-- inbox/outbox channels to communicate with it.
--
-- It might be simpler to have explicit state i.e.
-- acceptClient :: ServerTransport -> IO (TransportInterface, ServerTransport)
acceptClient :: StateT ServerTransport IO TransportInterface
acceptClient = do
	(clients, sock) <- get
	client <- lift $ acceptClient' (clients, sock)
	channels <- lift newTransportInterface
	put $ (M.insert client channels clients, sock)
	return channels

acceptClient' :: ServerTransport -> IO SockAddr
acceptClient' (clients, sock) = do
	(packet, client) <- receivePacket sock
	if Syn `elem` (getFlags packet) && M.notMember client clients
		then return client
		else acceptClient' (clients, sock)

-- | Start communications with clients connected to the given
-- server transport.
serveClients :: ServerTransport -> IO ()
serveClients (clients, sock) = do
	socketTVar <- atomically $ newTVar sock
	forkIO $ forever $ inboxLoop (M.map channelInbox clients) socketTVar
	forkIO $ forever $ outboxLoop (M.toList $ M.map channelOutbox clients) socketTVar
	return ()
	where
		inboxLoop inboxes socketTVar = do
			sock <- atomically $ readTVar socketTVar
			(packet, client) <- receivePacket sock
			case M.lookup client inboxes of
				Just inbox -> atomically $ writeTChan inbox (getPacketData packet)
				Nothing -> return ()

		outboxLoop outboxes socketTVar = do
			sock <- atomically $ readTVar socketTVar
			mapM_ (readAndSend sock) outboxes
			threadDelay 1000
			return ()

		readAndSend sock (addr, outbox) = do
			message <- atomically $ tryReadTChan outbox
			case message of
				Just m -> sendPacket sock (initialPacket m) addr
				Nothing -> return ()

newTransportInterface :: IO TransportInterface
newTransportInterface = do
	inbox <- atomically $ newTChan
	outbox <- atomically $ newTChan
	return $ TransportInterface
		{	channelInbox = inbox
		,	channelOutbox = outbox
		}

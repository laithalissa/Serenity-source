module Serenity.Network.Utility
(	TransportInterface(..)
,	Connection(..)
,	getTransportChannels
,	connectChannels
,	connectChannelsIO
,	listenChannels
,	listenChannelsIO
,	startListeningIO
,	readNTChan
,	readTChanUntilEmpty
,	sendMessages
) where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forever, liftM)

import Serenity.Network.Transport

import Serenity.Model.Message (Message)

data TransportInterface = TransportInterface
	{	channelInbox      :: TChan Message
	,	channelOutbox     :: TChan Message
	,	channelConnection :: TVar Connection
	}

newTransportInterface inbox outbox con = TransportInterface
	{	channelInbox      = inbox
	,	channelOutbox     = outbox
	,	channelConnection = con
	}

getTransportChannels :: (MonadTransport t) => t TransportInterface
getTransportChannels = do
	inbox  <- liftIO newTChanIO
	outbox <- liftIO newTChanIO
	connection <- getConnection
	connectionTvar <- liftIO $ newTVarIO connection
	liftIO $ forkIO $ forever $ inboxLoop inbox connectionTvar
	liftIO $ forkIO $ forever $ outboxLoop outbox connectionTvar
	return $ newTransportInterface inbox outbox connectionTvar
	where
		outboxLoop outbox connectionTvar = do
			(message, connection) <- atomically $ do
				message <- readTChan outbox
				connection <- readTVar connectionTvar
				return (message, connection)
			evalTransport (send message) connection

		inboxLoop inbox connectionTvar = do
			connection <- atomically $ readTVar connectionTvar
			message <- evalTransport (receive) connection
			atomically $ writeTChan inbox message

startListeningIO :: PortNumber -> IO Connection
startListeningIO port = evalTransport (do startListening port; getConnection) Unconnected

connectChannels :: (MonadTransport t) => String -> PortNumber -> t TransportInterface
connectChannels host port = do
	connect host port
	getTransportChannels

connectChannelsIO :: String -> PortNumber -> IO TransportInterface
connectChannelsIO host port = evalTransport (connectChannels host port) Unconnected

listenChannels :: (MonadTransport t) => t TransportInterface
listenChannels = do
	accept
	getTransportChannels

listenChannelsIO :: Connection -> IO TransportInterface
listenChannelsIO connection = evalTransport listenChannels connection

-- | Read the first n items from a TChan
readNTChan :: Int -> TChan a -> IO [a]
readNTChan n tchan = liftM (take n) (readTChanUntilEmpty tchan)

-- | Read all of the items from a TChan
readTChanUntilEmpty :: TChan a -> IO [a]
readTChanUntilEmpty chan = readTChanUntilEmpty' chan []

readTChanUntilEmpty' :: TChan a -> [a] -> IO [a]
readTChanUntilEmpty' tchan accum = do
	value <- atomically $ tryReadTChan tchan
	case value of
		Nothing -> return accum
		Just v -> readTChanUntilEmpty' tchan (accum ++ [v])

sendMessages :: TChan Message -> [Message] -> IO ()
sendMessages chan messages = atomically $ mapM_ (writeTChan chan) messages

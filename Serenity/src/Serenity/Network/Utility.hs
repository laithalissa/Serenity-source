module Serenity.Network.Utility where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

getTransportChannels :: Transport (TChan Message, TChan Message, TVar Connection)
getTransportChannels = do
	inbox  <- liftIO newTChanIO
	outbox <- liftIO newTChanIO
	connection <- getConnection
	connectionTvar <- liftIO $ newTVarIO connection
	liftIO $ forkIO $ forever $ inboxLoop inbox connectionTvar
	liftIO $ forkIO $ forever $ outboxLoop outbox connectionTvar
	return (inbox, outbox, connectionTvar)
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
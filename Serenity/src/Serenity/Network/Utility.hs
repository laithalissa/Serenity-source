module Serenity.Network.Utility where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

get_transport_channels :: Transport (TChan Message, TChan Message, TVar Connection)
get_transport_channels = do
	inbox  <- liftIO newTChanIO
	outbox <- liftIO newTChanIO
	connection <- get_connection
	connection_tvar <- liftIO $ newTVarIO connection
	liftIO $ forkIO $ forever $ inbox_loop inbox connection_tvar
	liftIO $ forkIO $ forever $ outbox_loop outbox connection_tvar
	return (inbox, outbox, connection_tvar)
	where
		outbox_loop outbox connection_tvar = do
			(message, connection) <- atomically $ do
				message <- readTChan outbox
				connection <- readTVar connection_tvar
				return (message, connection)
			eval_transport (send message) connection

		inbox_loop inbox connection_tvar = do
			connection <- atomically $ readTVar connection_tvar
			message <- eval_transport (receive) connection
			atomically $ writeTChan inbox message
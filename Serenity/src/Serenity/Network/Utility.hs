module Serenity.Network.Utility where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Serenity.Network.Transport

get_transport_channels :: Transport (TChan String, TChan String)
get_transport_channels = do
	inbox  <- liftIO newTChanIO
	outbox <- liftIO newTChanIO
	connection <- get_connection
	connection_tvar <- liftIO $ newTVarIO connection
	liftIO $ forkIO $ forever $ inbox_loop inbox connection_tvar
	liftIO $ forkIO $ forever $ outbox_loop outbox connection_tvar
	return (inbox, outbox)
	where
		outbox_loop outbox connection_tvar = do
			message <- liftIO $ atomically $ readTChan outbox
			connection <- atomically $ readTVar connection_tvar
			eval_transport (send message) connection

		inbox_loop inbox connection_tvar = do
			connection <- atomically $ readTVar connection_tvar
			message <- eval_transport (receive) connection
			atomically $ writeTChan inbox message
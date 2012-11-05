module Serenity.Network.Utility where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO)

import Serenity.Network.Transport

get_transport_channels :: Connection -> IO (TChan String, TChan String)
get_transport_channels connection = do
	inbox  <- newTChanIO
	outbox <- newTChanIO
	forkIO (inbox_loop connection inbox)
	forkIO (outbox_loop connection outbox)
	return (inbox, outbox)
	where
		inbox_loop = undefined
		outbox_loop = undefined
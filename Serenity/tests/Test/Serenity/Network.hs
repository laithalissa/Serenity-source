module Test.Serenity.Network (
	serverClientFixture
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent

serverClientFixture :: IO a -> IO () -> IO a
serverClientFixture serverBody client = do
	outputTvar <- atomically $ newEmptyTMVar
	forkIO $ server outputTvar
	forkIO client
	atomically $ readTMVar outputTvar 
	where
		server tvar = do
			input <- serverBody
			atomically $ putTMVar tvar $ input
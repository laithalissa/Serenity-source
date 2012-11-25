module Test.Serenity.Network (
	serverClientFixture
) where


import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent

readUntilJust :: TVar (Maybe a) -> IO a
readUntilJust tvar = atomically $ do
	maybeOutput <- readTVar tvar
	case maybeOutput of
		Nothing -> retry
		Just output -> return output

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
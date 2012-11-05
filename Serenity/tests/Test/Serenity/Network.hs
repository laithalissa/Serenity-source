module Test.Serenity.Network (
	server_client_fixture
) where


import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent

read_until_just :: TVar (Maybe a) -> IO a
read_until_just tvar = atomically $ do 
	maybe_output <- readTVar tvar
	case maybe_output of
		Nothing -> retry
		Just output -> return output

server_client_fixture :: IO a -> IO () -> IO a
server_client_fixture server_body client = do
	output_tvar <- atomically $ newTVar Nothing
	forkIO $ server output_tvar
	forkIO client
	read_until_just output_tvar where
		server tvar = do
			input <- server_body
			atomically $ writeTVar tvar $ Just input
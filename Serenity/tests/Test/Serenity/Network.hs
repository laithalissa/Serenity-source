module Test.Serenity.Network (test_group) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import Serenity.Network

test_group = testGroup "Network Tests" 
	[	testCase "Test a listening process accepts a connection" test_acceptance
	]

is_connected :: Connection -> Bool
is_connected Connected {} = True
is_connected _ = False

read_until_just :: TVar (Maybe a) -> IO a
read_until_just tvar = atomically $ do 
	maybe_output <- readTVar tvar
	case maybe_output of
		Nothing -> retry
		Just output -> return output

server_client_fixture server client input = do
	output_tvar <- atomically $ newTVar input
	forkIO $ server output_tvar
	forkIO client
	read_until_just output_tvar

test_acceptance = do
	connection <- server_client_fixture server client (Nothing :: Maybe Connection)
	True @=? (is_connected connection)
	where
		client :: IO ()
		client = do
			connection <- connect 9900
			return ()

		server :: TVar (Maybe Connection) -> IO ()
		server tvar = do
			connection <- listen 9900
			atomically $ writeTVar tvar $ Just connection
			return ()

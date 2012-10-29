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
	,	testCase "Test two processes can exchange info over a connection" test_send_receive
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

server_client_fixture :: IO a -> IO () -> IO a
server_client_fixture server_body client = do
	output_tvar <- atomically $ newTVar Nothing
	forkIO $ server output_tvar
	forkIO client
	read_until_just output_tvar where
		server tvar = do
			input <- server_body
			atomically $ writeTVar tvar $ Just input

test_acceptance = do
	connection <- server_client_fixture server client
	is_connected connection @?= True
	where
		client = do connect 9900; return ();
		server = do connection <- listen 9900; return connection

test_send_receive = do
	string <- server_client_fixture server client
	string @?= "some input"
	where
		client = do
			connection <- connect 9902
			--send "some input"
			return ()

		server = do
			connection <- listen 9902
			--receive
			return "some input"
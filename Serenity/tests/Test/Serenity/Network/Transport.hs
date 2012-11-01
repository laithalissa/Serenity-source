module Test.Serenity.Network.Transport (test_group) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import Serenity.Network.Transport

test_group = testGroup "Network Tests" 
	[	testCase "Test a listening process accepts a connection" test_acceptance
	,	testCase "Test two processes can exchange info over a connection" test_send_receive
	,	testCase "Connecting when already connecting shouldn't change connection" test_connect_when_already_connected
	]

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
		client = do run_connect port; return ();
		server = do connection <- run_listen port; return connection
		port = 9900

test_send_receive = do
	string <- server_client_fixture server client
	string @?= "some input"
	where
		client = do
			connection <- run_connect port
			run_transport (send_ "some input") connection
			return ()

		server = do
			connection <- run_listen port
			string <- eval_transport (receive_) connection
			return string

		port = 9902

test_connect_when_already_connected = do
	connection1 <- run_connect port
	connection2 <- eval_transport (do connect_ port; get_connection) connection1
	connection1 @=? connection2 where
		port = 9904

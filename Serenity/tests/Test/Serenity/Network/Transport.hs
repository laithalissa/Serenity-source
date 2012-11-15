module Test.Serenity.Network.Transport (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import Test.Serenity.Network (server_client_fixture)
import Serenity.Network.Transport
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Tests" 
	[	testCase "Test a listening process accepts a connection" test_acceptance
	,	testCase "Test two processes can exchange info over a connection" test_send_receive
	,	testCase "Connecting when already connecting shouldn't change connection" test_connect_when_already_connected
	]

test_acceptance = do
	connection <- server_client_fixture server client
	is_connected connection @?= True
	where
		client = do run_connect "localhost" port; return ();
		server = do connection <- run_listen port; return connection
		port = 9900

test_send_receive = do
	string <- server_client_fixture server client
	string @?= Message.Empty
	where
		client = do
			connection <- run_connect "localhost" port
			run_transport (send Message.Empty) connection
			return ()

		server = do
			connection <- run_listen port
			string <- eval_transport (receive) connection
			return string

		port = 9902

test_connect_when_already_connected = do
	connection1 <- run_connect "localhost" port
	connection2 <- eval_transport (do connect "localhost" port; get_connection) connection1
	connection1 @=? connection2 where
		port = 9904

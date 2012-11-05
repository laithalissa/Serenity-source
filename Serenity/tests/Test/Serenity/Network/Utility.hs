module Test.Serenity.Network.Utility (test_group) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Test.Serenity.Network (server_client_fixture)
import Serenity.Network.Transport
import Serenity.Network.Utility

test_group = testGroup "Network Utility Tests" 
	[	testCase "Test test_get_transport_channels returns two empty channels" test_get_transport_channels
	,	testCase "Test input placed in the input channel is sent" test_send_channel
	,	testCase "Test input sent arrives in the receive channel" test_receive_channel
	]

test_get_transport_channels = do
	connection <- run_connect "localhost" 9908
	(inbox, outbox) <- eval_transport get_transport_channels connection
	inbox_empty <- atomically $ isEmptyTChan inbox
	outbox_empty <- atomically $ isEmptyTChan outbox
	and [inbox_empty, outbox_empty] @?= True

test_send_channel = do
	string <- server_client_fixture server client
	string @?= "some input"
	where
		client = do
			connection <- run_connect "localhost" port
			(inbox, outbox) <- eval_transport get_transport_channels connection
			atomically $ writeTChan outbox "some input"
			return ()

		server = do
			connection <- run_listen port
			string <- eval_transport (receive) connection
			return string

		port = 9910

test_receive_channel = do
	string <- server_client_fixture server client
	string @?= "some input"
	where
		client = do
			connection <- run_connect "localhost" port
			run_transport (send "some input") connection
			return ()

		server = do
			connection <- run_listen port
			(inbox, outbox) <- eval_transport get_transport_channels connection
			string <- atomically $ readTChan inbox
			return string

		port = 9912
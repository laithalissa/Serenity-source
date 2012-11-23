module Test.Serenity.Network.Transport (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import Test.Serenity.Network (serverClientFixture)
import Serenity.Network.Transport
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Tests"
	[	testCase "Test a listening process accepts a connection" testAcceptance
	,	testCase "Test two processes can exchange info over a connection" testSendReceive
	,	testCase "Test two processes can exchange info over a connection both ways" testSendReceiveDuplex
	,	testCase "Connecting when already connecting shouldn't change connection" testConnectWhenAlreadyConnected
	]

testAcceptance = do
	connection <- serverClientFixture server client
	isConnected connection @?= True
	where
		client = do runConnect "localhost" port; return ();
		server = do connection <- runListen port; return connection
		port = 9900

testSendReceive = do
	message <- serverClientFixture server client
	message @?= Message.Empty
	where
		client = do
			connection <- runConnect "localhost" port
			runTransport (send Message.Empty) connection
			return ()

		server = do
			connection <- runListen port
			message <- evalTransport (receive) connection
			return message

		port = 9902

testSendReceiveDuplex = do
	message <- serverClientFixture server client
	message @?= Message.Empty
	where
		client = do
			connection <- runConnect "localhost" port
			runTransport (send Message.Empty) connection
			message <- evalTransport (receive) connection
			runTransport (send message) connection
			return ()

		server = do
			connection <- runListen port
			message <- evalTransport (receive) connection
			runTransport (send message) connection
			message2 <- evalTransport (receive) connection
			return message2

		port = 9898

testConnectWhenAlreadyConnected = do
	connection1 <- runConnect "localhost" port
	connection2 <- evalTransport (do connect "localhost" port; getConnection) connection1
	connection1 @=? connection2 where
		port = 9904

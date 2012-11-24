module Test.Serenity.Network.Utility (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Test.Serenity.Network (serverClientFixture)
import Serenity.Network.Transport
import Serenity.Network.Utility

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Utility Tests"
	[	testCase "Test testGetTransportChannels returns two empty channels" testGetTransportChannels
	,	testCase "Test input placed in the input channel is sent" testSendChannel
	,	testCase "Test input sent arrives in the receive channel" testReceiveChannel
	]

testGetTransportChannels = do
	connection <- runConnect "localhost" 9908
	TransportInterface inbox outbox conVar <- evalTransport getTransportChannels connection
	inboxEmpty <- atomically $ isEmptyTChan inbox
	outboxEmpty <- atomically $ isEmptyTChan outbox
	and [inboxEmpty, outboxEmpty] @?= True

testSendChannel = do
	string <- serverClientFixture server client
	string @?= Message.Empty
	where
		client = do
			TransportInterface inbox outbox conVar <- connectChannelsIO "localhost" port
			atomically $ writeTChan outbox Message.Empty
			return ()

		server = do
			connection <- runListen port
			string <- evalTransport (receive) connection
			return string

		port = 9910

testReceiveChannel = do
	string <- serverClientFixture server client
	string @?= Message.Empty
	where
		client = do
			connection <- runConnect "localhost" port
			runTransport (send Message.Empty) connection
			return ()

		server = do
			TransportInterface inbox outbox conVar <- listenChannelsIO port
			string <- atomically $ readTChan inbox
			return string

		port = 9912
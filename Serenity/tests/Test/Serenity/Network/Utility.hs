module Test.Serenity.Network.Utility (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.State

import Test.Serenity.Network (serverClientFixture)
import Serenity.Network.Server hiding (TransportInterface, receive, send)
import qualified Serenity.Network.Server as S (TransportInterface(..), receive, send)
import Serenity.Network.Transport
import Serenity.Network.Utility

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Utility Tests"
	[	testCase "Test testGetTransportChannels returns two empty channels" testGetTransportChannels
	,	testCase "Test input placed in the input channel is sent" testSendChannel
	,	testCase "Test input sent arrives in the receive channel" testReceiveChannel
	,	testCase "Test readNTChan" testReadNTChan
	,	testCase "Test readTChanUntilEmpty on empty TChan returns empty list" testReadUntilEmptyOnEmptyTChan
	,	testCase "Test readTChanUntilEmpty" testReadTChanUntilEmpty
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
			transport <- initTransport port
			(_, (_, sock)) <- runStateT acceptClient transport
			(message, _) <- S.receive sock
			return message

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
			transport <- initTransport port
			(client, transport') <- runStateT acceptClient transport
			serveClients transport'

			message <- atomically $ readTChan (S.channelInbox client)
			return message

		port = 9912

testReadNTChan = do
	tchan <- newTChanIO

	atomically $ writeTChan tchan 0
	atomically $ writeTChan tchan 1
	atomically $ writeTChan tchan 2

	items <- readNTChan 2 tchan
	[0, 1] @=? items

testReadUntilEmptyOnEmptyTChan = do
	tchan <- newTChanIO
	items <- readTChanUntilEmpty tchan 
	empty <- atomically $ isEmptyTChan tchan

	assertBool "TChan is not empty" empty
	assertBool "Items is not emtpy" (length items == 0)

testReadTChanUntilEmpty = do
	tchan <- newTChanIO

	atomically $ writeTChan tchan 0
	atomically $ writeTChan tchan 1
	atomically $ writeTChan tchan 2

	items <- readTChanUntilEmpty tchan
	empty <- atomically $ isEmptyTChan tchan

	assertBool "TChan is not empty" empty
	[0, 1, 2] @=? items

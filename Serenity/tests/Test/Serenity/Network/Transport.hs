module Test.Serenity.Network.Transport (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.Map as M (toList)

import Test.Serenity.Network (serverClientFixture)
import Serenity.Network.Connection
import Serenity.Network.Transport
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Tests"
	[	testCase "Test testGetTransportChannels returns two empty channels" testNewTransportInterface
	,	testCase "Test a listening process accepts a connection" testAcceptance
	,	testCase "Test input placed in the input channel is sent" testSendChannel
	,	testCase "Test input sent arrives in the receive channel" testReceiveChannel
	]

testNewTransportInterface = do
	interface <- newTransportInterface

	inboxEmpty <- atomically $ isEmptyTChan (channelInbox interface)
	outboxEmpty <- atomically $ isEmptyTChan (channelOutbox interface)
	assertBool "Inbox/outbox not empty." (and [inboxEmpty, outboxEmpty])

	connection <- atomically $ readTVar (channelConnection interface)
	connection @?= initialConnection

testAcceptance = do
	channels <- serverClientFixture server client
	connection <- atomically $ readTVar (channelConnection channels)
	assertBool "Connection not accepted." (isConnected connection)
	where
		client = do connect "localhost" port; return ()

		server = do
			transport <- listen port
			channels <- evalStateT acceptClient transport
			return channels

		port = 9900

testSendChannel = do
	maybeMessage <- serverClientFixture server client
	case maybeMessage of
		Just (message, _) -> message @?= Message.Empty
		Nothing -> assertFailure "No message received."
	where
		client = do
			TransportInterface inbox outbox conVar <- connectTo "localhost" port
			atomically $ writeTChan outbox Message.Empty

		server = do
			transport <- listen port
			(_, (clients, sock)) <- runStateT acceptClient transport
			receive clients sock

		port = 9910

testReceiveChannel = do
	string <- serverClientFixture server client
	string @?= Message.Empty
	where
		client = do
			(map, sock) <- connect "localhost" port
			let [(addr, channels)] = M.toList map
			send (channelConnection channels) sock Message.Empty addr

		server = do
			transport <- listen port
			(client, transport') <- runStateT acceptClient transport
			sendAndReceive transport'

			message <- atomically $ readTChan (channelInbox client)
			return message

		port = 9912

{-
testSendReceive = do
	maybeMessage <- serverClientFixture server client
	case maybeMessage of
		Just (message, _) -> message @?= Message.Empty
		Nothing -> assertFailure "No message received."
	where
		client = do
			(map, sock) <- connect "localhost" port
			let [(addr, channels)] = M.toList map
			send (channelConnection channels) Message.Empty sock addr

		server = do
			transport <- listen port
			(_, (clients, sock)) <- runStateT acceptClient transport
			message <- receive clients sock
			return message

		port = 9902

testSendReceiveDuplex = do
	message <- serverClientFixture server client
	assertMessage message Message.Empty
	where
		client = do
			(map, sock) <- connect "localhost" port
			let [(addr, channels)] = M.toList map

			send (channelConnection channels) Message.Empty sock addr
			message <- receive map sock
			assertMessage message Message.Empty
			send (channelConnection channels) (fst $ fromJust message) sock addr

		server = do
			-- Accept client connection
			transport <- listen port
			(_, (clients, sock)) <- runStateT acceptClient transport

			-- Exchange messages
			message <- receive clients sock
			assertMessage message Message.Empty
			send (sock message addr
			(message2, _) <- S.receive sock
			return message2

		assertMessage actual expected = do
			case actual of
				Just (message, _) -> message @?= expected
				Nothing -> assertFailure "No message received."

		port = 9898
-}

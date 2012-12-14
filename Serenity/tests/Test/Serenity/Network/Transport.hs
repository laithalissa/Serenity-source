module Test.Serenity.Network.Transport (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad.State
import Test.HUnit

import Test.Serenity.Network (serverClientFixture)
import Serenity.Network.Server hiding (receive, send)
import qualified Serenity.Network.Server as S (receive, send)
import Serenity.Network.Transport
import qualified Serenity.Network.Message as Message

tests = testGroup "Network Tests"
	-- [	testCase "Test a listening process accepts a connection" testAcceptance
	[	testCase "Test two processes can exchange info over a connection" testSendReceive
	,	testCase "Test two processes can exchange info over a connection both ways" testSendReceiveDuplex
	,	testCase "Connecting when already connecting shouldn't change connection" testConnectWhenAlreadyConnected
	]

{-
-- TODO reenable once server side connections are recreated
testAcceptance = do
	transportInterface <- serverClientFixture server client
	-- isConnected connection @?= True
	where
		client = do runConnect "localhost" port; return ();

		server = do
			transport <- initTransport port
			client <- evalStateT acceptClient transport
			return client

		port = 9900
-}

testSendReceive = do
	message <- serverClientFixture server client
	message @?= Message.Empty
	where
		client = do
			connection <- runConnect "localhost" port
			runTransport (send Message.Empty) connection
			return ()

		server = do
			transport <- initTransport port
			(_, (_, sock)) <- runStateT acceptClient transport
			(message, _) <- S.receive sock
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
			-- Accept client connection
			transport <- initTransport port
			(_, (_, sock)) <- runStateT acceptClient transport

			-- Exchange messages
			(message, addr) <- S.receive sock
			S.send sock message addr
			(message2, _) <- S.receive sock
			return message2

		port = 9898

testConnectWhenAlreadyConnected = do
	connection1 <- runConnect "localhost" port
	connection2 <- evalTransport (do connect "localhost" port; getConnection) connection1
	connection1 @=? connection2 where
		port = 9904

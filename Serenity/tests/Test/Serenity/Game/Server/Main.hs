module Test.Serenity.Game.Server.Main
(	tests
) where

import Test.Serenity.Network (serverClientFixture)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Control.Monad (replicateM_)

import Serenity.Game.Server.Main
import Serenity.Network.Transport
import Serenity.Network.Utility
import Serenity.Model.Message

tests = testGroup "Server Main Tests"
	[	testCase "Test that a client can connect using connectionPhase"  (testConnectionPhaseConnectsNClients 9920 1)
	,	testCase "Test that 3 clients can connect using connectionPhase" (testConnectionPhaseConnectsNClients 9921 3)
	]

testConnectionPhaseConnectsNClients port n = do
	(clientDataList, _) <- serverClientFixture server client
	length clientDataList @?= n
	where
		client = replicateM_ n (connect "localhost" port)
		server = connectionPhase port n

testSendToClient = do
	updates <- serverClientFixture client server
	let isUpdateEntity = case updates of 
		[UpdateEntity{}] -> True
		_ -> False
	isUpdateEntity @?= True
	where
		client = do
			TransportInterface inbox _ _ <- connectTo "localhost" port
			messages <- readTChanUntilEmpty inbox
			let updates = map (\(UpdateMessage m _ ) -> m) messages
			return updates

		server = do
			(clientDataList, _) <- connectionPhase port 1
			let updates = [UpdateEntity{}]
			sendToClients updates clientDataList
			return ()

		port = 9922

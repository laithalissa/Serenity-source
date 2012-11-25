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

import Control.Monad(replicateM)

import Serenity.Game.Server.Main
import Serenity.Network.Utility
import Serenity.Network.Message

tests = testGroup "Server Main Tests"
	[	testCase "Test that a client can connect using connectionPhase"  (testConnectionPhaseConnectsNClients 1 9920)
	,	testCase "Test that 3 clients can connect using connectionPhase" (testConnectionPhaseConnectsNClients 3 9921)
	]

testConnectionPhaseConnectsNClients n port = do
	clientDataList <- serverClientFixture server client
	length clientDataList @?= n
	where
		client = do
			replicateM n (do connectChannelsIO "localhost" port)
			return ()

		server = do
			clientDataList <- connectionPhase n port
			return clientDataList

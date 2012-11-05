module Test.Serenity.Network.Utility (test_group) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Serenity.Network.Transport
import Serenity.Network.Utility

test_group = testGroup "Network Utility Tests" 
	[	testCase "Test test_get_transport_channels returns two empty channels" test_get_transport_channels
	]

test_get_transport_channels = do
	connection <- run_connect "localhost" 9908
	(inbox, outbox) <- get_transport_channels connection
	inbox_empty <- atomically $ isEmptyTChan inbox
	outbox_empty <- atomically $ isEmptyTChan outbox
	and [inbox_empty, outbox_empty] @?= True
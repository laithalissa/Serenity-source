module Test.Serenity.Network.Utility (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.State

import Serenity.Network.Utility

tests = testGroup "Network Utility Tests"
	[	testCase "Test readNTChan" testReadNTChan
	,	testCase "Test readTChanUntilEmpty on empty TChan returns empty list" testReadUntilEmptyOnEmptyTChan
	,	testCase "Test readTChanUntilEmpty" testReadTChanUntilEmpty
	]

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

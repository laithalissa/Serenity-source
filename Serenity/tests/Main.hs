module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Test.Serenity.Game.Server.Main as ServerMain
import qualified Test.Serenity.Network.Transport as Transport
import qualified Test.Serenity.Network.Utility as Network_Utility

main = defaultMain tests

tests =
	[	testGroup "Top Level"
		[	testCase "A test of tests" (1 @=? 1)
		]
	,	ServerMain.test_group
	,	Transport.test_group
	,	Network_Utility.test_group
	]
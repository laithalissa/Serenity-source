module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Test.Serenity.Network.Transport as Transport

main = defaultMain tests

tests =
	[	testGroup "Top Level"
		[	testCase "A test of tests" (1 @=? 1)
		]
	,	Transport.test_group
	]
module Main where

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified Test.Serenity.Game.Server.Main as ServerMain
import qualified Test.Serenity.Network.Transport as Transport
import qualified Test.Serenity.Network.Utility as Network_Utility
import qualified Test.Serenity.Network.Message as Message
import qualified Test.Serenity.Network.Packet as Packet

main = defaultMainWithArgs allTests htf_args

htf_args = 
	[	"--maximum-generated-tests=5000"
	,	"--maximum-unsuitable-generated-tests=3000"
	]

allTests =
	[	testGroup "Top Level"
		[	testCase "A test of tests" (1 @=? 1)
		]
	,	Packet.tests
	,	Transport.tests
	,	Network_Utility.tests
	,	Message.tests
	,	ServerMain.tests
	]
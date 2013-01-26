module Main where

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import qualified Test.Serenity.Network.Transport as Transport
import qualified Test.Serenity.Network.Utility as Network_Utility
import qualified Test.Serenity.Model.Message as Message
import qualified Test.Serenity.Network.Packet as Packet
import qualified Test.Serenity.Game.Server.Main as ServerMain
--import qualified Test.Serenity.Game.Shared.GameStateUpdate as GameStateUpdate
--import qualified Test.Serenity.Game.Server.GameStateTransform as GST
-- import qualified Test.Serenity.Game.Server.Network as ServerNetwork
import qualified Test.Serenity.Game.Client.KeyboardState as KeyboardState
import qualified Test.Serenity.Maths.Util as MathUtil

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
	-- ,	GameStateUpdate.tests
	-- ,	GST.tests
	-- ,	ServerNetwork.networkTests
	,	KeyboardState.tests
	,	MathUtil.tests
	]

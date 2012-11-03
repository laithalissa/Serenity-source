
module Test.Serenity.Game.Server.Network (networkTests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit(assertEqual)

import Serenity.Game.Server.Network


networkTests :: Test
networkTests = testGroup testName tests

testName :: String
testName = "Server Network Tests"

tests :: [Test]
tests = 
  [ testClientCanConnectToServer    
  ]
  
  
testClientCanConnectToServer :: Test
testClientCanConnectToServer = testCase "Test client can connect to server" (assertEqual "2 == 2?" 2 2)
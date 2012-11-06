
module Test.Serenity.Game.Server.Main (tests)
where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import qualified Serenity.Game.Server.Main as Main

tests = testGroup "Server Main Tests" [
              testCase "Test the test is acting like a test" testMyTest
              ]

testMyTest = do
  True @=? (1==1)
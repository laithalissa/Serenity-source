{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Network.Message (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Data.DeriveTH
import Data.Derive.Arbitrary

import Serenity.Network.Message
import Data.Binary (Binary(..), encode, decode)



$(derive makeArbitrary ''ShipOrder)
$(derive makeArbitrary ''ShipOrderState)
$(derive makeArbitrary ''GameEntity)
$(derive makeArbitrary ''Entity)
$(derive makeArbitrary ''Message)
$(derive makeArbitrary ''Update)
$(derive makeArbitrary ''Command)

tests = testGroup "Network Utility Tests"
	[	testProperty "Test converting to binary and back returns the same message" propertyGetPutIsID
	]

propertyGetPutIsID :: Message -> Bool
propertyGetPutIsID message = (decode.encode) message == message
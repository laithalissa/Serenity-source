{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Model.Message (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Data.DeriveTH
import Data.Derive.Arbitrary

import Serenity.Model.Message
import Serenity.Model.Entity
import Data.Binary (Binary(..), encode, decode)

-- $(derive makeArbitrary ''ShipOrder 
-- $(derive makeArbitrary ''ShipOrderState)
-- $(derive makeArbitrary ''GameEntity)
$(derive makeArbitrary ''Order)
$(derive makeArbitrary ''Damage)
$(derive makeArbitrary ''Ship)
$(derive makeArbitrary ''Entity)
$(derive makeArbitrary ''Message)
$(derive makeArbitrary ''Update)
$(derive makeArbitrary ''Command)

tests = testGroup "Network Message Tests"
	[	testProperty "Test converting to binary and back returns the same message" propertyGetPutIsID
	]

propertyGetPutIsID :: Message -> Bool
propertyGetPutIsID message = (decode.encode) message == message

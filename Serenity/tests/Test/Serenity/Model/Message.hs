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
import Serenity.Model.Sector
import Data.Binary (Binary(..), encode, decode)

derive makeArbitrary ''Command
derive makeArbitrary ''Damage
derive makeArbitrary ''Entity
derive makeArbitrary ''Goal
derive makeArbitrary ''Message
derive makeArbitrary ''Order
derive makeArbitrary ''Resources
derive makeArbitrary ''Ship
derive makeArbitrary ''ShipAction
derive makeArbitrary ''ShipConfiguration
derive makeArbitrary ''ShipType
derive makeArbitrary ''SystemUpgrade
derive makeArbitrary ''Update
derive makeArbitrary ''Weapon
derive makeArbitrary ''WeaponEffect

tests = testGroup "Network Message Tests"
	[	testProperty "Test converting to binary and back returns the same message" propertyGetPutIsID
	]

propertyGetPutIsID :: Message -> Bool
propertyGetPutIsID message = (decode.encode) message == message

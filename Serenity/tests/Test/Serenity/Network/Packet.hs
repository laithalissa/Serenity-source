{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Network.Packet (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Data.DeriveTH
import Data.Derive.Arbitrary

import Serenity.Network.Packet
import Serenity.Network.Message (Message(Empty))

import Data.Set (Set)
import qualified Data.Set as Set

$(derive makeArbitrary ''Flag)

tests = testGroup "Network Packet Tests" 
	[	testProperty "Test converting flags to binary and back works correctly" propertyFlagsAreSetCorrectly
	]

propertyFlagsAreSetCorrectly :: [Flag] -> Bool
propertyFlagsAreSetCorrectly flags = Set.fromList (getFlags $ setFlags flags (initialPacket Empty)) == Set.fromList flags
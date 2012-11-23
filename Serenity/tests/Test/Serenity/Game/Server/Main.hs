module Test.Serenity.Game.Server.Main
(	tests
) where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Serenity.Game.Server.Main
import Serenity.Network.Message

tests = testGroup "Server Main Tests"
	[	testProperty "Test updateWorld with id" propertyUpdateWorldOnID
	,	testProperty "Test updateWorld passes time" propertyUpdateWorldOnTime
	,	testProperty "Test updateWorld is id with no message" propertyUpdateWorldOnMessage
	,	testProperty "Test updateWorld with a message" propertyUpdateWorldOnNoMessage
	]

propertyUpdateWorldOnID :: Int -> Bool
propertyUpdateWorldOnID world =
	(fst $ updateWorld (\_ -> \w -> w) (\_ -> \w -> (w, [])) world [] 0) == world

propertyUpdateWorldOnTime :: Double -> Bool
propertyUpdateWorldOnTime world =
	(fst $ updateWorld (\_ -> \w -> w) (\time -> \w -> (w+time, [])) world [] 1) > world

increaseIfMessage [] = id
increaseIfMessage [Empty] = (+1)

propertyUpdateWorldOnMessage :: Int -> Bool
propertyUpdateWorldOnMessage   world =
	(fst $ updateWorld increaseIfMessage (\_ -> \w -> (w, [])) world [Empty] 0) > world

propertyUpdateWorldOnNoMessage :: Int -> Bool
propertyUpdateWorldOnNoMessage world =
	(fst $ updateWorld increaseIfMessage (\_ -> \w -> (w, [])) world [] 0) == world

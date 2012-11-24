{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Game.Shared.GameStateUpdate
(	tests
) where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck

import Serenity.Game.Shared.GameStateUpdate(gameStateUpdate)

import qualified Serenity.Game.Shared.Model.GameState as GameState
import Serenity.Game.Shared.Model.GameState(GameState(..))


import Serenity.Game.Shared.Model.GameMap(GameMap(..), Planet(..), SpaceLane(..), exampleGameMap)
import Serenity.Game.Shared.Model.Entity(GameEntity(..), Entity(..))
import Serenity.Game.Shared.Model.ShipOrder(ShipOrder(..))
import Serenity.Game.Shared.Model.Common(Resources(..))

import Serenity.Network.Message(Update(..))

import Data.Set(Set)

import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)


-- $( derive makeArbitrary ''GameState )
$( derive makeArbitrary ''GameMap )
$( derive makeArbitrary ''Planet )
$( derive makeArbitrary ''SpaceLane )
$( derive makeArbitrary ''Entity )
$( derive makeArbitrary ''ShipOrder )
$( derive makeArbitrary ''Resources )



tests = testGroup "GameStateUpdate Tests"
	[	testProperty "Test someting" testSomething
	]


testSomething :: Int -> Bool
testSomething seed = (gameStateUpdate gameState (UpdateEntity gameEntity)) /= gameState
	where
		gameState = GameState.initialize exampleGameMap
		gameEntity = GameEntity 0 "joseph" entity
		entity = System 1

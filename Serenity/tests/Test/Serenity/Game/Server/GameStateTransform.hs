{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Game.Server.GameStateTransform
(	tests
) where


import Test.Framework (testGroup)
--import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Concurrent
import Test.QuickCheck
import Test.QuickCheck.Property

import Serenity.Game.Server.GameStateTransform
	(	Update(..)
	,	Command(..)
	,	ShipOrder(..)
	,	GameEntity(..)
	,	Entity(..)
	,	GameEntity(..)
	,	transform
	)

import Serenity.Game.Shared.Model.GameState
	(	GameState(gameStateEntities)
	,	initialize
	,	addEntity
	,	hasEntityId
	)

import Serenity.Game.Shared.Model.GameState(hasEntityId)

import Serenity.Network.Message(updateEntity)


import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)
import qualified Data.Set as Set

import Serenity.Game.Shared.Model.GameMap(exampleGameMap)

$( derive makeArbitrary ''Command )
$( derive makeArbitrary ''ShipOrder )

tests = testGroup "GameStateTransform Tests"
	[	testProperty 
			"test the update contains the entity specified and that entity has the correct order" 
			testUpdateContainsEntitySpecifiedWithCorrectOrder
	]

testUpdateContainsEntitySpecifiedWithCorrectOrder :: Command -> Property
testUpdateContainsEntitySpecifiedWithCorrectOrder command@(GiveOrder eId order) = 
	(hasEntityId eId gameState) ==> conjoin 
		[	(length updates) == 1
		,	((==) eId . entityId . updateEntity) (updates !! 0)
		]
	where
		updates :: [Update]
		updates = transform command gameState


gameState = foldl f (initialize exampleGameMap) entities
	where
		f = flip addEntity
		entities = 
			[	createGameEntity 0 (5,5) StayStillOrder
			,	createGameEntity 1 (10,5) StayStillOrder
			,	createGameEntity 2 (90,45) MoveOrder{moveOrderLocation=(10,10)}
			]

		createGameEntity eid location order = 
			GameEntity
			{	entityId=eid
			,	ownerId="Joseph"
			,	entity=createEntity location order
			}
			
		createEntity location order = 	
			Ship
			{	shipClass=1
			,	shipLocation=location
			,	shipDirection=(0,1)
			,	shipSpeed=(0,1)
			,	shipOrder=order
			}

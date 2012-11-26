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
	,	ShipOrderState(..)
	,	GameEntity(..)
	,	Entity(..)
	,	GameEntity(..)
	,	transform
	,	step
	,	nextLocation
	)

import Serenity.Game.Shared.Model.GameState
	(	GameState(gameStateEntities)
	,	initialize
	,	addEntity
	,	hasEntityId
	)

import Serenity.Game.Shared.Model.GameMap
	(	GameMap(..)
	,	Planet(..)
	,	SpaceLane(..)
	)

import Serenity.Game.Shared.Model.Common
	(	Resources(..)
	,	Location
	,	Speed
	)

import Serenity.Game.Shared.Model.GameState(hasEntityId)

import Serenity.Network.Message(updateEntity)


import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)
import qualified Data.Set as Set
import Data.List(and)

import Serenity.Game.Shared.Model.GameMap(exampleGameMap)

$( derive makeArbitrary ''Command )
$( derive makeArbitrary ''ShipOrder )
$( derive makeArbitrary ''ShipOrderState )
$( derive makeArbitrary ''GameEntity )
$( derive makeArbitrary ''Entity )
$( derive makeArbitrary ''GameMap )
$( derive makeArbitrary ''Planet)
$( derive makeArbitrary ''SpaceLane )
$( derive makeArbitrary ''Resources )


tests = testGroup "GameStateTransform Tests"
	[	testProperty 
			"test the transform returns update which contains the entity specified and that entity has the correct order" 
			testTransformReturnsUpdateWithEntityInCorrectState
--	,	testProperty
--			"test step doesn't do anything when 0 time has passed"
--			testStepDoesNotMoveEntityWhenNoTimeZero
	,	testProperty
			"test nextLocation outputs same location if no time has passed"
			testNextLocationDoesNotChangeLocationWhenTimeIsZero
	]

testTransformReturnsUpdateWithEntityInCorrectState :: Command -> Property
testTransformReturnsUpdateWithEntityInCorrectState command@(GiveOrder eId order) = 
	(hasEntityId eId gameState) ==> conjoin 
		[	(length updates) == 1
		,	((==) eId . entityId . updateEntity) (updates !! 0)
		]
	where
		updates :: [Update]
		updates = transform command gameState

--testStepDoesNothingWhenNoTimePassed :: Command -> Property
--testStepDoesNothingWhenNoTimePassed command@(GiveOrder eId order) =
	

testStepMovesShipTowardsTarget :: Property
testStepMovesShipTowardsTarget = conjoin
	[	(distance (90,45) (10,10)) > (distance (movingShipLocation (step 1 gameState)) (10, 10))	
	,	floatComp 0.0 (distance (movingShipLocation (step 1000 gameState)) (10, 10))
	]
	where
		movingShipLocation updates = (shipLocation . entity . updateEntity) (foldl (chooseShip 2) (head updates) updates)
		distance (x1,y1) (x2,y2) = ((x1-x2)**2 + (y1-y2)**2)**0.5
		chooseShip eId a b = if eId == (getId a) then a else b
		getId = entityId . updateEntity

testStepDoesNotMoveEntityWhenNoTimeZero :: GameMap -> [GameEntity] -> Property
testStepDoesNotMoveEntityWhenNoTimeZero gameMap entities = 
	((Set.size . Set.fromList . map entityId) entities == (length entities)) ==>
	(conjoin $ zipWith compareGE entities newEntities)
		where
		compareGE a b = (f a) == (f b)
			where 
				f e@(GameEntity{entity=Ship{}}) = (shipLocation . entity) e
				f e = (-1.0, -1.0)
				
		swapOut ge@(GameEntity eId oId entity) = (getEntityFromUpdates eId)
		newEntities = map swapOut entities
		gs = generateGameState gameMap entities
		updates = step 0 gs
		getEntityFromUpdates eId = updateEntity ((filter ((eId==) . entityId . updateEntity) updates) !! 0)
		


testNextLocationDoesNotChangeLocationWhenTimeIsZero :: Location -> Speed -> Bool
testNextLocationDoesNotChangeLocationWhenTimeIsZero location speed = 
	(nextLocation location speed 0) == location



generateGameState :: GameMap -> [GameEntity] -> GameState
generateGameState gameMap entities = foldl (flip addEntity) (initialize gameMap) entities


gameState = foldl f (initialize exampleGameMap) entities
	where
		f = flip addEntity
		entities = 
			[	createGameEntity 0 (5,5) StayStillOrderState
			,	createGameEntity 1 (10,5) StayStillOrderState
			,	createGameEntity 2 (90,45) (MoveOrderState [(10,10)])
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
			,	shipOrderState=order
			}


class FullEq a where
	fullEq :: a -> a -> Bool


floatComp :: Float -> Float -> Bool
floatComp a b = (a-b) < 0.0001

instance FullEq Entity where
	fullEq
		Ship
		{	shipClass=shipClassA
		,	shipLocation=(xA, yA)
		,	shipDirection=shipDirectionA
		,	shipSpeed=shipSpeedA
		,	shipOrderState=shipOrderStateA
		} 
	
		Ship
		{	shipClass=shipClassB
		,	shipLocation=(xB, yB)
		,	shipDirection=shipDirectionB
		,	shipSpeed=shipSpeedB
		,	shipOrderState=shipOrderStateB
		}
		= and
		[	shipClassA == shipClassB
		,	(floatComp xA xB) && (floatComp yA yB)
		,	shipDirectionA == shipDirectionB
		,	shipSpeedA == shipSpeedB
		,	shipOrderStateA == shipOrderStateB
		]

	fullEq
		Gun
		{	shipId=shipIdA
		,	weaponSlotIndex=weaponSlotIndexA
		}

		Gun
		{	shipId=shipIdB
		,	weaponSlotIndex=weaponSlotIndexB
		}
		= and
		[	shipIdA == shipIdB
		,	weaponSlotIndexA == weaponSlotIndexB
		]

	fullEq 
		System{shipId=shipIdA} 
		System{shipId=shipIdB} = shipIdA == shipIdB

	fullEq

		Bullet
		{	bulletLocation=bulletLocationA
		,	bulletDirection=bulletDirectionA
		,	bulletSpeed=bulletSpeedA
		}

		Bullet
		{	bulletLocation=bulletLocationB
		,	bulletDirection=bulletDirectionB
		,	bulletSpeed=bulletSpeedB
		}	
		= and
		[	bulletLocationA == bulletLocationB
		,	bulletDirectionA == bulletDirectionB
		,	bulletSpeedA == bulletSpeedB
		]

	entityA `fullEq` entityB = False

instance FullEq GameEntity where
	fullEq
		GameEntity
		{	entityId=entityIdA
		,	ownerId=ownerIdA
		,	entity=entityA
		} 

		GameEntity
		{	entityId=entityIdB
		,	ownerId=ownerIdB
		,	entity=entityB
		}
		= and
		[	entityIdA == entityIdB
		,	ownerIdA == ownerIdB
		,	entityA `fullEq` entityB
		]


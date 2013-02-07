{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Game.Shared.GameStateUpdate
(	tests
) where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe(fromJust)

import Control.Concurrent
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Property(Property, (==>))

import Serenity.Game.Shared.GameStateUpdate(updateGameState)

import Serenity.Game.Shared.Model.GameState(GameState(..), hasEntity, initialize, addEntity, getEntityById)


import Serenity.Game.Shared.Model.GameMap(GameMap(..), Planet(..), SpaceLane(..), exampleGameMap)
import Serenity.Game.Shared.Model.Entity(GameEntity(..), Entity(..))
import Serenity.Game.Shared.Model.ShipOrder(ShipOrder(..), ShipOrderState(..))
import Serenity.Game.Shared.Model.Common(Resources(..))

import Serenity.Model.Message(Update(..), Update(..))

import Data.Set(Set(..))

import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)


-- $( derive makeArbitrary ''GameState )
$( derive makeArbitrary ''GameMap )
$( derive makeArbitrary ''Planet )
$( derive makeArbitrary ''SpaceLane )
$( derive makeArbitrary ''Entity )
$( derive makeArbitrary ''GameEntity )
$( derive makeArbitrary ''ShipOrder )
$( derive makeArbitrary ''ShipOrderState )
$( derive makeArbitrary ''Resources )
-- $( derive makeArbitrary ''Set )



tests = testGroup "GameStateUpdate Tests"
	[	testProperty "Test entity added to Game State if not already in Game State" testEntityAddedCorrectlyIfNotAlreadyInGameState
	,	testProperty "Test entity added to Game State which already has entity with that id is not changed" testEntityAddedIsIgnoredIfEntityAlreadyInGameState
	]


testEntityAddedCorrectlyIfNotAlreadyInGameState :: GameMap -> [GameEntity] -> GameEntity -> Property
testEntityAddedCorrectlyIfNotAlreadyInGameState gameMap entities entity = (not $ hasEntity entity gsBefore) ==> (hasEntity entity gsAfter)  
	where
		gsBefore = createGameState gameMap entities
		gsAfter = updateGameState (AddEntity entity) gsBefore


testEntityAddedIsIgnoredIfEntityAlreadyInGameState :: GameMap -> [GameEntity] -> GameEntity -> Property
testEntityAddedIsIgnoredIfEntityAlreadyInGameState gameMap entities entity = 
	(hasEntity entity gsBefore) && ((ownerId entity) /= (ownerId oldEntity)) ==> 
		(gsBefore == gsAfter) && ((ownerId $ fromJust $ getEntityById (entityId entity) gsAfter) == (ownerId oldEntity))
		
	where
		gsBefore = createGameState gameMap entities
		gsAfter = updateGameState (AddEntity entity) gsBefore
		oldEntity = fromJust $ getEntityById (entityId entity) gsBefore 
		



---------- helper functions ----------

createGameState :: GameMap -> [GameEntity] -> GameState
createGameState gameMap entities = foldl (\gm e -> addEntity e gm) (initialize gameMap) entities





module Serenity.Game.Shared.GameStateUpdate
(	module Serenity.Game.Shared.Model.GameState
,	module Serenity.Network.Message
,	updateGameState
,	manyUpdateGameState
) where



import Serenity.Game.Shared.Model.GameState
	(	GameState
	,	addEntity
	,	removeEntity
	,	hasEntityId
	)

import Serenity.Network.Message
	(	Update(..)
	)

import Serenity.Game.Shared.Model.Entity
	(	GameEntity(..)
	)

updateGameState ::  Update -> GameState -> GameState
updateGameState (AddEntity entity) gameState = 
	if hasEntityId (entityId entity) gameState 
		then gameState 
		else addEntity entity gameState
updateGameState (UpdateEntity entity) gameState = addEntity entity gameState
updateGameState (DeleteEntity entity) gameState = removeEntity entity gameState


manyUpdateGameState :: [Update] -> GameState -> GameState
manyUpdateGameState updates state = foldl (flip updateGameState) state updates



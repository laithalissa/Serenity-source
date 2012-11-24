
module Serenity.Game.Shared.GameStateUpdate
(	module Serenity.Game.Shared.Model.GameState
,	module Serenity.Network.Message
,	gameStateUpdate
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


gameStateUpdate :: GameState -> Update -> GameState
gameStateUpdate gameState (AddEntity entity) = 
	if hasEntityId (entityId entity) gameState 
		then gameState 
		else addEntity entity gameState

gameStateUpdate gameState (UpdateEntity entity) = addEntity entity gameState
gameStateUpdate gameState (DeleteEntity entity) = removeEntity entity gameState








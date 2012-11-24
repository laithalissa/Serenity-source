
module Serenity.Game.Shared.GameStateUpdate
(	module Serenity.Game.Shared.Model.GameState
,	module Serenity.Network.Message
,	gameStateUpdate
) where



import Serenity.Game.Shared.Model.GameState
	(	GameState
	,	addEntity
	,	removeEntity
	)

import Serenity.Network.Message
	(	Update(..)
	)

gameStateUpdate :: GameState -> Update -> GameState
gameStateUpdate gameState (AddEntity entity) = addEntity entity gameState
gameStateUpdate gameState (UpdateEntity entity) = addEntity entity gameState
gameStateUpdate gameState (DeleteEntity entity) = removeEntity entity gameState








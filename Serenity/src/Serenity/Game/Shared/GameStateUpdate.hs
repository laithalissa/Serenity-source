
module Serenity.Game.Shared.GameStateUpdate
(	module Serenity.Game.Model.GameState(GameState)
,	module Serenity.Network.Message(Update)
,	gameStateUpdate
) where


import Serenity.Game.Model.GameState(GameState)
import Serenity.Network.Message(Update)

gameStateUpdate :: GameState -> Update -> GameState
gameStateUpdate gameState update = gameState
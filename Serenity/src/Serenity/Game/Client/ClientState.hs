module Serenity.Game.Client.ClientState where

import Serenity.Game.Client.UIState

data ClientState = ClientState
	{ messages :: [String] -- XXX should be Messages
	, uiState :: UIState ClientState
	-- gameState :: GameState
	}
	deriving Show

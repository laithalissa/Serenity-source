module Serenity.Game.Client.World where

import Serenity.Game.Client.UIState

data World = World
	{ messages :: [String] -- XXX should be Messages
	, uiState :: UIState World
	}
	deriving Show

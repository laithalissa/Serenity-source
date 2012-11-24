module Serenity.Game.Client.UIState where

import Serenity.Game.Client.Common (ViewPort)

import Serenity.Sheen.View

data UIState a = UIState
	{	views :: View a
	,	viewPort :: ViewPort
	}

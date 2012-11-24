module Serenity.Game.Client.UIState where

import Serenity.Game.Shared.Model.Common

import Serenity.Sheen.View

data UIState a = UIState
	{	views :: View a
	,	viewPort :: ViewPort
	}

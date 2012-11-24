module Serenity.Game.Client.ClientMessage where

import Serenity.Network.Message (Command)

import Serenity.Game.Client.Common
import Serenity.Game.Shared.Model.Common

data ClientMessage = ClientMessageGUI GUICommand
	| ClientMessageCommand Command
	deriving (Show, Eq)

data GUICommand = ClientScroll ViewPortMove
	deriving (Show, Eq)
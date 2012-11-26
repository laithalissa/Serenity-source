module Serenity.Game.Client.ClientMessage where

import Serenity.Network.Message (Command)

import Serenity.Game.Client.ClientState

data ClientMessage = ClientMessageGUI GUICommand
	| ClientMessageCommand Command
	deriving (Show, Eq)

data GUICommand = ClientScroll ViewPortMove
	| ClientZoom ViewPortZoom
	deriving (Show, Eq)

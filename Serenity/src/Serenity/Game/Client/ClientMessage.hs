module Serenity.Game.Client.ClientMessage where

import Serenity.Model.Message (Command)

import Serenity.Game.Client.ClientState

data ClientMessage = 
	  ClientMessageGUI GUICommand
	| ClientMessageCommand Command

data GUICommand = 
	  ClientScroll ViewPortMove
	| ClientZoom ViewPortZoom
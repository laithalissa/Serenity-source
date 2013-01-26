module Serenity.Game.Client.Controller

where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.Pure.Game (Event)

import Serenity.Game.Client.ClientMessage (ClientMessage(..))
import Serenity.Game.Client.ClientState
import qualified Serenity.Game.Client.InputFilter as InputFilter
import qualified Serenity.Game.Client.GUI as GUI

import Serenity.Model
import Serenity.Sheen.View

import Control.Lens

render :: ClientState -> Picture
render clientState = GUI.render (clientState^.clientGame) (clientState^.clientUIState) (clientState^.clientAssets)

handleInput :: Event -> ClientState -> ClientState
handleInput event clientState =
	case InputFilter.handleInput event clientState of
		[] -> clientState
		clientMessages -> foldl (flip handleMessage) clientState clientMessages

handleMessage :: ClientMessage -> ClientState -> ClientState
handleMessage (ClientMessageGUI gCommand)    = clientUIState  %~ (GUI.handleMessage gCommand)
handleMessage (ClientMessageCommand command) = clientCommands %~ (++ [command])

mainView :: View ClientState
mainView = makeView "main" (0, fst windowSize, 0, snd windowSize)
module Serenity.Game.Client.InputFilter where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

import Serenity.Game.Client.ClientMessage (ClientMessage(..), GUICommand(..))
import Serenity.Game.Client.ClientState (ClientState)
import qualified Serenity.Game.Client.Logic as Logic

import GHC.Float

handleInput :: Event -> ClientState -> [ClientMessage]
handleInput event clientState = case event of
	(EventKey (MouseButton button) Down _ (x,y)) -> case button of
		WheelUp -> [ClientMessageGUI $ ClientZoom 1]
		WheelDown -> [ClientMessageGUI $ ClientZoom (-1)]
		LeftButton -> Logic.handleClick (float2Double x, float2Double y) clientState
		_ -> []

	(EventKey (Char key) Down _ _) -> case key of
		-- Scrolling
		'w' -> [ClientMessageGUI $ ClientScroll (0, 10) ]
		'a' -> [ClientMessageGUI $ ClientScroll (-10, 0)]
		's' -> [ClientMessageGUI $ ClientScroll (0, -10)]
		'd' -> [ClientMessageGUI $ ClientScroll (10, 0) ]

		-- Zooming
		'q' -> [ClientMessageGUI $ ClientZoom 1]
		'e' -> [ClientMessageGUI $ ClientZoom (-1)]

		_ -> []

	_ -> []


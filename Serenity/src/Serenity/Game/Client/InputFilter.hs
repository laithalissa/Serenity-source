module Serenity.Game.Client.InputFilter
(	InputFilter
,	initialize
,	handleInput
) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

import Serenity.Game.Client.ClientMessage (ClientMessage(..), GUICommand(..))
import Serenity.Game.Client.ClientState (ClientState)
import qualified Serenity.Game.Client.Logic as Logic

data InputFilter = InputFilter
	deriving (Show, Eq)

initialize :: InputFilter
initialize = InputFilter

handleInput :: Event -> ClientState -> [ClientMessage]
handleInput event clientState = case event of
	(EventKey (MouseButton button) Down _ point) -> case button of
		LeftButton -> Logic.handleClick point clientState

		_ -> []

	(EventKey (Char key) Down _ _) -> case key of
		-- Scrolling
		'w' -> [ClientMessageGUI $ ClientScroll (0, 10)]
		'a' -> [ClientMessageGUI $ ClientScroll (-10, 0)]
		's' -> [ClientMessageGUI $ ClientScroll (0, -10)]
		'd' -> [ClientMessageGUI $ ClientScroll (10, 0)]

		-- Zooming
		'q' -> [ClientMessageGUI $ ClientZoom (5, 5, -10, -10)]
		'e' -> [ClientMessageGUI $ ClientZoom (-5, -5, 10, 10)]

		_ -> []

	_ -> []


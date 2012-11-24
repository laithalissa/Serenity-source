module Serenity.Game.Client.InputFilter
(	InputFilter
,	initialize
,	handleInput
) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

import Serenity.Game.Client.ClientMessage (ClientMessage(..), GUICommand(..))
import qualified Serenity.Game.Client.Logic as Logic

import Serenity.Game.Shared.Model.Common (OwnerId)
import Serenity.Game.Shared.Model.GameState (GameState)

data InputFilter = InputFilter
	deriving (Show, Eq)

initialize :: InputFilter
initialize = InputFilter

handleInput :: Event -> GameState -> OwnerId -> InputFilter -> ([ClientMessage], InputFilter)
handleInput event gameState player inputFilter = (clientMessages, inputFilter)
	where
		clientMessages = case event of
			(EventKey (MouseButton button) Down _ point) -> case button of
				LeftButton -> Logic.handleClick point gameState player

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


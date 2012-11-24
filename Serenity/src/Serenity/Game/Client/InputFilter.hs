module Serenity.Game.Client.InputFilter
(	InputFilter
,	initialize
,	handleInput
) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), KeyState(..))

import Serenity.Game.Client.ClientMessage (ClientMessage(..), GUICommand(..))

data InputFilter =
	InputFilter
	deriving (Show, Eq)

initialize :: InputFilter
initialize = InputFilter

handleInput :: Event -> inputFilter -> (Maybe ClientMessage, inputFilter)
handleInput event inputFilter = (clientMessage, inputFilter)
	where
		clientMessage = case event of
			(EventKey key Down _ _) -> case key of
				Char 'w' -> Just $ ClientMessageGUI $ ClientScroll (0, 10)
				Char 'a' -> Just $ ClientMessageGUI $ ClientScroll (-10, 0)
				Char 's' -> Just $ ClientMessageGUI $ ClientScroll (0, -10)
				Char 'd' -> Just $ ClientMessageGUI $ ClientScroll (10, 0)
				_ -> Nothing

			_ -> Nothing


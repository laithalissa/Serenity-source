module Serenity.Game.Client.InputFilter
(	InputFilter
,	initialize
,	handleInput
) where

import Graphics.Gloss.Interface.Pure.Game
	(	SpecialKey(..)
	,	Key(..)
	,	Event(..)
	,	KeyState(..)
	,	MouseButton(..)
	)

import Serenity.Game.Client.ClientMessage
	(	ClientMessage(..)
	,	GraphicsMessage(..)
	,	WorldMessage(..)
	)

data InputFilter =
	InputFilter
	deriving (Show, Eq)

initialize :: InputFilter
initialize = InputFilter

handleInput :: Event -> inputFilter -> (Maybe ClientMessage, inputFilter)
handleInput event inputFilter = (Just $ handleInput event, inputFilter)
	where
		handleInput (EventKey key keyState modifiers mouse) = case key of
			Char 'w' -> (ClientMessageGraphics $ ClientScroll (10,10,50,50))
			Char _ -> (ClientMessageGraphics $ ClientScroll (0,0,100,100))
		handleInput _ = (ClientMessageGraphics $ ClientScroll (0, 0, 100, 100))



module Serenity.Game.Server.InputFilter
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

import Serenity.Game.Model.ClientMessage
	(	ClientMessage(..)
	,	GraphicsMessage(..)
	,	WorldMessage(..)
	)


initialize :: InputFilter 
handleInput :: Event -> inputFilter -> (Maybe ClientMessage, inputFilter)

data InputFilter =
	InputFilter  	        	          
	deriving (Show, Eq)        

initialize = InputFilter

handleInput event inputFilter = (Just $ handleInput event, inputFilter) 
	where
		handleInput (EventKey key keyState modifiers mouse) = case key of
			Char 'w' -> (ClientMessageGraphics $ ClientScroll (10,10,50,50))
			Char _ -> (ClientMessageGraphics $ ClientScroll (0,0,100,100))
		handleInput _ = (ClientMessageGraphics $ ClientScroll (0, 0, 100, 100))


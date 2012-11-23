module Serenity.Sheen.UIEvent
	( UIEvent(..)
	, eventToUIEvent
	)
where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), Point)

data UIEvent = ViewClick Point MouseButton
	-- mouse movement, widget clicks, mouse drag (?)
	--

eventToUIEvent :: Event -> Maybe UIEvent
eventToUIEvent (EventKey (MouseButton LeftButton) Down _ point) = Just $ ViewClick point LeftButton
eventToUIEvent _ = Nothing

module Serenity.Sheen.UIEvent
(	UIEvent(..)
,	event2UIEvent
,	translateUIEvent
,	translateEvent
) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), Modifiers(..), Point)

data UIEvent =
	  UIEventKey Point Key KeyState Modifiers
	| UIEventMotion Point
	| UIEventFocusGained
	| UIEventFocusLost

event2UIEvent :: Event -> UIEvent
event2UIEvent (EventKey key keystate mods point) = UIEventKey point key keystate mods
event2UIEvent (EventMotion point) = UIEventMotion point

translateUIEvent :: Int -> Int -> UIEvent -> UIEvent
translateUIEvent x y (UIEventKey (clickX, clickY) key keystate mods) = (UIEventKey newPoint key keystate mods)
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))

translateEvent :: Int -> Int -> Event -> Event
translateEvent x y (EventKey key keystate mods (clickX, clickY)) = (EventKey key keystate mods newPoint)
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))
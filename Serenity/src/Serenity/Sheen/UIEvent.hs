module Serenity.Sheen.UIEvent where

import Control.Lens
import Graphics.Gloss.Interface.Pure.Game

--data UIEvent =
--	  UIEventKey Point Key KeyState Modifiers
--	| UIEventMotion Point
--	| UIEventFocusGained
--	| UIEventFocusLost
--	| UIEventMouseOverInside
--	| UIEventMouseOverOutside

data UIEvent = 
	  UIEventMouseUpInside   MouseButton Point Modifiers
	| UIEventMouseUpOutside  MouseButton Point Modifiers
	| UIEventMouseDownInside MouseButton Point Modifiers
	| UIEventKeyPress Key KeyState Modifiers
	| UIEventMotion Point
	| UIEventFocusGained
	| UIEventFocusLost
	| UIEventMouseOverInside
	| UIEventMouseOverOutside
	deriving Eq

data UIEvents = UIEvents 
	{	_uieventsEventUnderMouse     :: [UIEvent]
	,	_uieventsEventLastMouseDown  :: [UIEvent]
	,	_uieventsEventToFocus        :: [UIEvent]
	,	_uieventsUpdateFocus         :: Bool
	,	_uieventsUpdateMouseOver     :: Bool
	,	_uieventsUpdateMouseDown     :: Bool
	}

makeLenses ''UIEvents

event2UIEvents :: Event -> ((Float, Float), UIEvents)
event2UIEvents (EventKey (MouseButton mouseButton) Down mods point) = (,) point UIEvents
	{	_uieventsEventUnderMouse     = [UIEventMouseDownInside mouseButton point mods]
	,	_uieventsEventLastMouseDown  = []
	,	_uieventsEventToFocus        = []
	,	_uieventsUpdateFocus         = True
	,	_uieventsUpdateMouseOver     = False
	,	_uieventsUpdateMouseDown     = True
	}
event2UIEvents (EventKey (MouseButton mouseButton) Up mods point) = (,) point UIEvents
	{	_uieventsEventUnderMouse     = [UIEventMouseUpInside mouseButton point mods]
	,	_uieventsEventLastMouseDown  = [UIEventMouseUpOutside mouseButton point mods]
	,	_uieventsEventToFocus        = []
	,	_uieventsUpdateFocus         = False
	,	_uieventsUpdateMouseOver     = False
	,	_uieventsUpdateMouseDown     = False
	}
event2UIEvents (EventKey key keystate mods point) = (,) point UIEvents
	{	_uieventsEventUnderMouse     = []
	,	_uieventsEventLastMouseDown  = []
	,	_uieventsEventToFocus        = [UIEventKeyPress key keystate mods]
	,	_uieventsUpdateFocus         = False
	,	_uieventsUpdateMouseOver     = False
	,	_uieventsUpdateMouseDown     = False
	}
event2UIEvents (EventMotion point) = (,) point UIEvents
	{	_uieventsEventUnderMouse     = [UIEventMotion point]
	,	_uieventsEventLastMouseDown  = []
	,	_uieventsEventToFocus        = []
	,	_uieventsUpdateFocus         = False
	,	_uieventsUpdateMouseOver     = True
	,	_uieventsUpdateMouseDown     = False
	}

--translateUIEvent :: Int -> Int -> UIEvent -> UIEvent
--translateUIEvent x y (UIEventKey (clickX, clickY) key keystate mods) = (UIEventKey newPoint key keystate mods)
--	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))
--translateUIEvent x y (UIEventMotion (clickX, clickY)) = UIEventMotion newPoint
--	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))
--translateUIEvent _ _ event = event

translateEvent :: Int -> Int -> Event -> Event
translateEvent x y (EventKey key keystate mods (clickX, clickY)) = (EventKey key keystate mods newPoint)
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))
translateEvent x y (EventMotion (clickX, clickY)) = EventMotion newPoint
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))
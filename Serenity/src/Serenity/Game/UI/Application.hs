module Serenity.Game.UI.Application where

import Serenity.Sheen

serenityVersionString = "Alpha 1.01"

data ApplicationMode = Splash | Menu | Host| Join | Lobby | Play | Quit deriving Eq

initMenuButton string action = 
	initButton 
	(initLabel (StaticString string) black (Just (changeAlpha (light $ bright $ bright green) 0.7))) {_labelScale = 2.1}
	(initLabel (StaticString string) black (Just $ changeAlpha (dark yellow) 0.7)) {_labelScale = 2.1}
	[(ButtonEvent LeftButton Up (Modifiers Up Up Up), action)]
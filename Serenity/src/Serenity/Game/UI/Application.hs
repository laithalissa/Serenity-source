{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Application where

import Serenity.Sheen

import Control.Lens

serenityVersionString = "Alpha 1.01"

data ApplicationMode = Splash | Menu | Host| Join | Lobby | Play | Quit deriving Eq

buttonColor = black
buttonBackground = (changeAlpha (light $ bright $ bright green) 0.7)
buttonPressedBackground = changeAlpha (dark yellow) 0.7

initMenuButton string action = 
	initButton 
	(initLabel (StaticString string) buttonColor (Just buttonBackground)       ) {_labelScale = 2.1}
	(initLabel (StaticString string) buttonColor (Just buttonPressedBackground)) {_labelScale = 2.1}
	[(ButtonEventMouseUpInside LeftButton (Modifiers Up Up Up), action)]

initMenuLabel string = initLabel (StaticString string) buttonColor (Just buttonBackground)

initMenuTextBox :: Simple Lens a String -> TextBox a
initMenuTextBox lens = initTextBox lens buttonColor (Just buttonBackground) buttonPressedBackground 2.1

initMenuTextBoxLabel :: String -> Simple Lens a String -> TextBoxLabel a
initMenuTextBoxLabel string lens = initTextBoxLabel string lens buttonColor (Just buttonBackground) buttonPressedBackground 2.1
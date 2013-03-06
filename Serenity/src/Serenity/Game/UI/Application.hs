{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Application where

import Serenity.Sheen
import Serenity.External

import Control.Lens
import Data.Char

serenityVersionString = "Alpha 1.01"

data ApplicationMode = Splash | Menu | Host| Join | Lobby | Play | Quit deriving Eq

background assets = Just $ translate 0 60 $ getPicture "background" assets

buttonColor = black
buttonBackground = (changeAlpha (light $ bright $ bright green) 0.7)
buttonPressedBackground = changeAlpha (dark yellow) 0.7
translucentBackground = changeAlpha (greyN 0.1) 0.7

initBox bounds = (initView bounds)
	{	_viewBackground = Just $ translucentBackground
	}

initMenuButton string action = 
	initButton 
	(initLabel (StaticString string) buttonColor (Just buttonBackground)       ) {_labelScale = 1.6}
	(initLabel (StaticString string) buttonColor (Just buttonPressedBackground)) {_labelScale = 1.6}
	[(ButtonEventMouseUpInside LeftButton (Modifiers Up Up Up), action)]

initMenuLabel string = initLabel (StaticString string) buttonColor (Just buttonBackground)

initMenuTextBox :: Simple Lens a String -> TextBox a
initMenuTextBox lens = initTextBox lens buttonColor (Just buttonBackground) buttonPressedBackground 1.6

initMenuTextBoxLabel :: String -> Simple Lens a String -> TextBoxLabel a
initMenuTextBoxLabel string lens = initTextBoxLabel string lens buttonColor (Just buttonBackground) buttonPressedBackground 1.6

nameValidation :: String -> String
nameValidation = filter (flip elem $ ['a'..'z']++['A'..'Z']++"_")

portValidation :: String -> String
portValidation = filter isDigit

numPlayersValidation :: String -> String
numPlayersValidation = (\x -> case x of _:_ -> [last x]; _ -> "").(filter isDigit)
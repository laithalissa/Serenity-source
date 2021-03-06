{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Application where

import Serenity.Sheen
import Serenity.External

import Control.Lens
import Data.Char

class AppState a where
	aMode :: Simple Lens a ApplicationMode
	aAssets :: Simple Lens a Assets

serenityVersionString = "Alpha 1.2"

data ApplicationMode = Splash | Menu | Credits | Host| Join | Quick | FleetB | Lobby | Play | End | Quit deriving Eq

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

initMenuTextBox :: TextBox a
initMenuTextBox = initTextBox buttonColor (Just buttonBackground) buttonPressedBackground 1.6

initMenuTextBoxLabel :: String -> TextBoxLabel a
initMenuTextBoxLabel string = initTextBoxLabel string buttonColor (Just buttonBackground) buttonPressedBackground 1.6

nameValidation :: String -> String
nameValidation = filter (flip elem $ ['a'..'z']++['A'..'Z']++['0'..'9']++"_")

fileNameValidation :: String -> String
fileNameValidation = filter (flip notElem "\"*/:<>?\\|")

portValidation :: String -> String
portValidation = filter isDigit

numPlayersValidation :: String -> String
numPlayersValidation = (\x -> case x of _:_ -> [last x]; _ -> "").(filter isDigit)

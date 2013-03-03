{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.TextBox where

import Serenity.Sheen.View
import Serenity.Sheen.Views.Label
import Serenity.Sheen.UIEvent

import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import Control.Monad.State
import Data.Char

data TextBox a = TextBox
	{	_tbFocus :: Bool
	,	_tbLabel :: Label a
	,	_tbFocusBackground :: Color
	,	_tbEnabled :: Bool
	,	_tbPostEdit :: String -> String
	}
makeLenses ''TextBox

initTextBox :: Simple Lens a String -> Color -> Maybe Color -> Color -> Float -> TextBox a
initTextBox valueLens color backg focusBackg scale = TextBox
	{	_tbFocus = False
	,	_tbLabel = (initLabel (DynamicString valueLens) color backg) {_labelScale = scale}
	,	_tbFocusBackground = focusBackg
	,	_tbEnabled = True
	,	_tbPostEdit = id
	}

textBox :: a -> Simple Lens a (TextBox a) -> Simple Lens a String -> ((Int, Int), (Int, Int)) -> View a
textBox a tb aString bounds = (label a (tb.tbLabel) bounds)
	{	_viewBackground = if a^.tb.tbFocus 
			then Just $ a^.tb.tbFocusBackground
			else a^.tb.tbLabel.labelBackground
	,	_viewEventHandler = Just $ \event -> if (a^.tb.tbEnabled) && ((a^.tb.tbFocus) || event == UIEventFocusGained)
			then (aString %~ a^.tb.tbPostEdit) $ tbEventHandler tb aString event a
			else a
	}

tbEventHandler :: Simple Lens a (TextBox a) -> Simple Lens a String -> UIEvent -> a -> a
tbEventHandler tb aString (UIEventKeyPress (Char k)                  Up (Modifiers Up   Up Up)) = aString %~ (++[k])
tbEventHandler tb aString (UIEventKeyPress (Char k)                  Up (Modifiers Down Up Up)) = aString %~ (++[toUpper k])
tbEventHandler tb aString (UIEventKeyPress (SpecialKey KeySpace)     Up (Modifiers _    Up Up)) = aString %~ (++" ")
tbEventHandler tb aString (UIEventKeyPress (SpecialKey KeyBackspace) Up (Modifiers _    Up Up)) = aString %~ init'
tbEventHandler tb aString (UIEventKeyPress (SpecialKey KeyDelete)    Up (Modifiers _    Up Up)) = aString %~ init'
tbEventHandler tb aString UIEventFocusGained = tb.tbFocus .~ True
tbEventHandler tb aString UIEventFocusLost   = tb.tbFocus .~ False
tbEventHandler _ _ _ = id

init' list@(_:_) = init list
init' [] = []
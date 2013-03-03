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
	,	_viewEventHandler = Just $ tbEventHandler a tb aString
	}

tbEventHandler :: a -> Simple Lens a (TextBox a) -> Simple Lens a String -> UIEvent -> a 
tbEventHandler a tb aString event = 
	if (a^.tb.tbEnabled) && ((a^.tb.tbFocus) || event == UIEventFocusGained)
		then (aString %~ a^.tb.tbPostEdit) $ tbEventUpdate tb aString event a
		else a

tbEventUpdate :: Simple Lens a (TextBox a) -> Simple Lens a String -> UIEvent -> a -> a
tbEventUpdate tb aString (UIEventKeyPress (Char '\b')               Up (Modifiers Up   Up Up)) = aString %~ init'
tbEventUpdate tb aString (UIEventKeyPress (Char k)                  Up (Modifiers Up   Up Up)) = aString %~ (++[k])
tbEventUpdate tb aString (UIEventKeyPress (Char k)                  Up (Modifiers Down Up Up)) = aString %~ (++[toUpper k])
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeySpace)     Up (Modifiers _    Up Up)) = aString %~ (++" ")
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeyBackspace) Up (Modifiers _    Up Up)) = aString %~ init'
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeyDelete)    Up (Modifiers _    Up Up)) = aString %~ init'
tbEventUpdate tb aString UIEventFocusGained = tb.tbFocus .~ True
tbEventUpdate tb aString UIEventFocusLost   = tb.tbFocus .~ False
tbEventUpdate _ _ _ = id

init' [] = []
init' xs = init xs

data TextBoxLabel a = TextBoxLabel
	{	_tblLabel   :: Label a
	,	_tblTextBox :: TextBox a
	}
makeLenses ''TextBoxLabel

_tblPostEdit = _tbPostEdit . _tblTextBox

tblPostEdit :: Simple Lens (TextBoxLabel a) (String -> String)
tblPostEdit  =  tblTextBox .  tbPostEdit

initTextBoxLabel :: String -> Simple Lens a String -> Color -> Maybe Color -> Color -> Float -> TextBoxLabel a
initTextBoxLabel labelString valueLens color backg focusBackg scale = TextBoxLabel
	{	_tblLabel   = (initLabel (StaticString labelString) color backg) {_labelScale = scale}
	,	_tblTextBox = (initTextBox valueLens color backg focusBackg scale)
	}

textBoxLabel :: a -> Simple Lens a (TextBoxLabel a) -> Simple Lens a String -> ((Int, Int), (Int, Int)) -> Int -> View a
textBoxLabel a tbl aString bounds@((x,y),(dx,dy)) width = (initView bounds)
	{	_viewSubviewMode  = ViewSubviewModeKeep
	,	_viewEventHandler = Just $ tbEventHandler a (tbl.tblTextBox) aString 
	} <++
	[	(label a (tbl.tblLabel)  ((0,0),(width-1,dy)) )
	,	(textBox a (tbl.tblTextBox) aString ((width,0),(dx-width,dy)) )
	]

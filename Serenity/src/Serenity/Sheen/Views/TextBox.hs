{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.TextBox where

import Serenity.Sheen.View
import Serenity.Sheen.Views.Label
import Serenity.Sheen.UIEvent

import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import Data.Char

data TextBox a = TextBox
	{	_tbFocus :: Bool
	,	_tbColor :: Color
	,	_tbBackgroundColor :: Maybe Color
	,	_tbFocusBackground :: Color
	,	_tbDisabledBackground :: Color
	,	_tbScale :: Float
	,	_tbEnabled :: a -> Bool
	,	_tbPostEdit :: String -> String
	}
makeLenses ''TextBox

initTextBox :: Color -> Maybe Color -> Color -> Float -> TextBox a
initTextBox color backg focusBackg scale = TextBox
	{	_tbFocus = False
	,	_tbColor = color
	,	_tbBackgroundColor = backg
	,	_tbScale = scale
	,	_tbFocusBackground = focusBackg
	,	_tbDisabledBackground = greyN 0.3
	,	_tbEnabled = \_ -> True
	,	_tbPostEdit = id
	}

textBox :: a -> Simple Lens a (TextBox a) -> Simple Lens a String -> ((Int, Int), (Int, Int)) -> View a
textBox a tb aString bounds = (labelStatic a ((initLabel (DynamicString aString) (a^.tb.tbColor) (a^.tb.tbBackgroundColor)) & (labelScale .~ (a^.tb.tbScale))) bounds)
	{	_viewBackground = if not (a^.tb.tbEnabled $ a)
		then Just $ a^.tb.tbDisabledBackground
		else if a^.tb.tbFocus 
			then Just $ a^.tb.tbFocusBackground
			else a^.tb.tbBackgroundColor
	,	_viewEventHandler = Just $ tbEventHandler a tb aString
	}

tbEventHandler :: a -> Simple Lens a (TextBox a) -> Simple Lens a String -> UIEvent -> a 
tbEventHandler a tb aString event = 
	if (a^.tb.tbEnabled $ a) && ((a^.tb.tbFocus) || event == UIEventFocusGained)
		then (aString %~ a^.tb.tbPostEdit) $ tbEventUpdate tb aString event a
		else a

tbEventUpdate :: Simple Lens a (TextBox a) -> Simple Lens a String -> UIEvent -> a -> a
tbEventUpdate tb aString (UIEventKeyPress (Char '\b')               Down (Modifiers Up   Up Up)) = aString %~ init'
tbEventUpdate tb aString (UIEventKeyPress (Char k)                  Down (Modifiers Up   Up Up)) = aString %~ (++[k])
tbEventUpdate tb aString (UIEventKeyPress (Char k)                  Down (Modifiers Down Up Up)) = aString %~ (++[toUpper k])
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeySpace)     Down (Modifiers _    Up Up)) = aString %~ (++" ")
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeyBackspace) Down (Modifiers _    Up Up)) = aString %~ init'
tbEventUpdate tb aString (UIEventKeyPress (SpecialKey KeyDelete)    Down (Modifiers _    Up Up)) = aString %~ init'
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
_tblEnabled  = _tbEnabled  . _tblTextBox

tblPostEdit :: Simple Lens (TextBoxLabel a) (String -> String)
tblPostEdit = tblTextBox . tbPostEdit

tblEnabled :: Simple Lens (TextBoxLabel a) (a -> Bool)
tblEnabled = tblTextBox . tbEnabled

initTextBoxLabel :: String -> Color -> Maybe Color -> Color -> Float -> TextBoxLabel a
initTextBoxLabel labelString color backg focusBackg scale = TextBoxLabel
	{	_tblLabel   = (initLabel (StaticString labelString) color backg) {_labelScale = scale}
	,	_tblTextBox = (initTextBox color backg focusBackg scale)
	}

textBoxLabel :: a -> Simple Lens a (TextBoxLabel a) -> Simple Lens a String -> ((Int, Int), (Int, Int)) -> Int -> View a
textBoxLabel a tbl aString bounds@((x,y),(dx,dy)) width = (initView bounds)
	{	_viewSubviewMode  = ViewSubviewModeKeep
	,	_viewEventHandler = Just $ tbEventHandler a (tbl.tblTextBox) aString 
	} <++
	[	(label a (tbl.tblLabel)  ((0,0),(width-1,dy)) ) 
			& (if (a^.tbl.tblEnabled $ a) then id else viewBackground .~ (Just $ a^.tbl.tblTextBox.tbDisabledBackground))
	,	(textBox a (tbl.tblTextBox) aString ((width,0),(dx-width,dy)) )
	]

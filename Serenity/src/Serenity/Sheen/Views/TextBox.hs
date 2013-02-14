{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Serenity.Sheen.Views.TextBox where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import GHC.Float

data TextBox = TextBox
	{	_tbValue :: String
	,	_tbFocus :: Bool
	,	_tbColor :: Color
	,	_tbBackground :: Maybe Color
	,	_tbScale :: Double
	}
makeLenses ''TextBox

textBox :: a -> Simple Lens a TextBox -> Simple Lens a String -> ((Int, Int), (Int, Int)) -> View a
textBox a tb store bounds = (initView bounds)
	{	_viewDepict = Just $ Translate 3 3 $ Color color $ Scale scale scale $ Text value
	,	_viewBackground = backg
	} where
		value = a^.tb.tbValue
		scale = double2Float $ 0.1 * a^.tb.tbScale
		color = a^.tb.tbColor
		backg = a^.tb.tbBackground

-- TODO TextBox logic
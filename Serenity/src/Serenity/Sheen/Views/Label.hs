{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Serenity.Sheen.Views.Label where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import GHC.Float

data Label = Label
	{	_labelValue :: String
	,	_labelColor :: Color
	,	_labelBackground :: Maybe Color
	,	_labelScale :: Double
	}
makeLenses ''Label

label :: a -> Getter a Label -> ((Int, Int), (Int, Int)) -> View a
label a label ((xmin, ymin), (xsize, ysize)) = (initView ((xmin, ymin), (xsize, ysize)))
	{	_viewDepict = Just $ Translate 3 3 $ Color color $ Scale scale scale $ Text value
	,	_viewBackground = backg
	} where
		value = a^.label.labelValue
		scale = double2Float $ 0.1 * a^.label.labelScale
		color = a^.label.labelColor
		backg = a^.label.labelBackground
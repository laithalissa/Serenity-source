{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.Label where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

data LabelValue a = StaticString String | forall s. Show s => DynamicString (Simple Lens a s) | NoString

data Label a = Label
	{	_labelValue :: LabelValue a
	,	_labelColor :: Color
	,	_labelBackground :: Maybe Color
	,	_labelScale :: Float
	}

initLabel :: LabelValue a -> Color -> Maybe Color -> Label a
initLabel value color backg = Label
	{	_labelValue = value
	,	_labelColor = color
	,	_labelBackground = backg
	,	_labelScale = 1
	}

makeLenses ''Label

label :: a -> Getter a (Label a) -> ((Int, Int), (Int, Int)) -> View a
label a label bounds = (initView bounds)
	{	_viewDepict = do
			v <- value
			return $ Translate 3 3 $ Color color $ Scale scale scale $ Text v
	,	_viewBackground = backg
	} where
		scale = 0.1 * a^.label.labelScale
		color = a^.label.labelColor
		backg = a^.label.labelBackground
		value = case a^.label.labelValue of
			StaticString s -> Just s
			DynamicString lens -> Just $ show (a^.lens)
			NoString -> Nothing
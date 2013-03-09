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
	,	_labelTextOffset :: (Float, Float)
	}

initLabel :: LabelValue a -> Color -> Maybe Color -> Label a
initLabel value color backg = Label
	{	_labelValue = value
	,	_labelColor = color
	,	_labelBackground = backg
	,	_labelScale = 1
	,	_labelTextOffset = (6,6)
	}

makeLenses ''Label

label :: a -> Getter a (Label a) -> ((Int, Int), (Int, Int)) -> View a
label a label bounds = (initView bounds)
	{	_viewDepict = do
			v <- value
			return $ Translate x y $ Color color $ Scale scale scale $ Text v
	,	_viewBackground = backg
	} where
		scale = 0.1 * a^.label.labelScale
		(x,y) = a^.label.labelTextOffset
		color = a^.label.labelColor
		backg = a^.label.labelBackground
		value = case a^.label.labelValue of
			StaticString s -> Just s
			DynamicString lens -> Just $ (showFix.show) (a^.lens)
			NoString -> Nothing

labelStatic :: a -> (Label a) -> ((Int, Int), (Int, Int)) -> View a
labelStatic a l bounds = label a (to (\_ -> l)) bounds

-- This is a bit of a hack
showFix :: String -> String
showFix [] = []
showFix s = start ++ middle ++ end where
	start = if head s == '\"' then "" else [head s]
	middle = (tail.init) s
	end = if last s == '\"' then "" else [last s]

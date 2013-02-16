{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Serenity.Sheen.Views.PictureView where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

data PictureValue a = StaticPicture Picture | forall p. Drawable p => DynamicPicture (Simple Lens a p)

data PictureView a = PictureView
	{	_pictureViewValue :: PictureValue a
	,	_pictureViewScale :: Float
	}

initPictureView :: PictureValue a -> PictureView a
initPictureView value = PictureView
	{	_pictureViewValue = value
	,	_pictureViewScale = 1
	}

makeLenses ''PictureView

pictureView :: a -> Getter a (PictureView a) -> ((Int, Int), (Int, Int)) -> View a
pictureView a pictureView bounds = (initView bounds)
	{	_viewDepict = Just $ Scale scale scale $ picture
	} where
		scale = a^.pictureView.pictureViewScale
		picture = case a^.pictureView.pictureViewValue of
			StaticPicture p -> p
			DynamicPicture lens -> render (a^.lens)

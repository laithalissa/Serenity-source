{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Splash where

import Serenity.Sheen
import Serenity.Game.UI.Application

import Control.Lens

data SplashData a = SplashData
	{	_splashTime :: Float
	,	_splashLabel :: Label a
	}

makeLenses ''SplashData

initSplashData :: Simple Lens a (SplashData a) -> SplashData a
initSplashData aSplash = SplashData
	{	_splashTime = 0
	,	_splashLabel = initLabel (GenericLabel (aSplash.splashTime)) green Nothing
	}

viewSplash :: a -> Simple Lens a (SplashData a) -> View a
viewSplash a aData = (initView ((0, 0), (1024, 768))) <++
	[	label a (aData.splashLabel) ((300,400),(110,16))
	]

timeSplash :: Simple Lens a (SplashData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeSplash aData aMode dt a = scale $ mode $ time $ a 
	where
		time  = aData.splashTime +~ dt
		mode  = if a^.aData.splashTime < 8 then id else aMode.~Menu
		scale = aData.splashLabel.labelScale .~ a^.aData.splashTime
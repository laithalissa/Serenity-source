{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Menu where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External

import Control.Lens

data MenuData a = MenuData
	{	_menuLabel :: Label a
	}

makeLenses ''MenuData

initMenuData :: Simple Lens a (MenuData a) -> Assets -> MenuData a
initMenuData aMenu assets = MenuData
	{	_menuLabel = (initLabel (StaticString "Main Menu !") black (Just green)){_labelScale = 2}
	}

viewMenu :: a -> Simple Lens a (MenuData a) -> Simple Lens a Assets -> View a
viewMenu a aData aAssets = (initView ((0, 0), (1024, 768))) 
	{	_viewDepict = Just $ getPicture "background" (a^.aAssets)
	} <++
	[	label a (aData.menuLabel) ((600,600),(220,30))
	]

timeMenu :: Simple Lens a (MenuData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeMenu aData aMode dt a = a
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Menu where

import Serenity.Sheen
import Serenity.Game.UI.Application

import Control.Lens

data MenuData a = MenuData
	{	_menuLabel :: Label a
	}

makeLenses ''MenuData

initMenuData :: Simple Lens a (MenuData a) -> MenuData a
initMenuData aMenu = MenuData
	{	_menuLabel = (initLabel (StringLabel "Main Menu !") black (Just yellow)){_labelScale = 2}
	}

viewMenu :: a -> Simple Lens a (MenuData a) -> View a
viewMenu a aData = (initView ((0, 0), (1024, 768))) 
	{	_viewBackground = Just (greyN 0.1)
	} <++
	[	label a (aData.menuLabel) ((600,600),(220,30))
	]

timeMenu :: Simple Lens a (MenuData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeMenu aData aMode dt a = a
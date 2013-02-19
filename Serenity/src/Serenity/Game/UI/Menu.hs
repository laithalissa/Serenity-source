{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Menu where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External

import Control.Lens

data MenuData a = MenuData
	{	_menuTitleLabel   :: Label a
	,	_menuVersionLabel :: Label a
	,	_menuHostButton   :: Button a ApplicationMode
	,	_menuJoinButton   :: Button a ApplicationMode
	,	_menuQuitButton   :: Button a ApplicationMode
	}

makeLenses ''MenuData

initMenuData :: Assets -> MenuData a
initMenuData assets = MenuData
	{	_menuTitleLabel   = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_menuVersionLabel = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_menuHostButton   = initMenuButton "Host    ->" (\_ -> Host)
	,	_menuJoinButton   = initMenuButton "Join    ->" (\_ -> Join)
	,	_menuQuitButton   = initMenuButton "Quit    -<" (\_ -> Quit)
	}

viewMenu :: a -> Simple Lens a (MenuData a) -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewMenu a aData aAssets aMode = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = Just $ getPicture "background" (a^.aAssets)
	}	<++
	[	label a (aData.menuTitleLabel) ((30,650),(220,30))
	,	label a (aData.menuVersionLabel) ((0,0),(100,15))
	,	(initView ((680, 0), (345, 750))) 
		{	_viewBackground = Just $ changeAlpha (greyN 0.1) 0.7
		}	<++
		[	button a (aData.menuHostButton) aMode ((80,650),(185,28))
		,	button a (aData.menuJoinButton) aMode ((80,550),(185,28))
		,	button a (aData.menuQuitButton) aMode ((80, 50),(185,28))
		]
	]

timeMenu :: Simple Lens a (MenuData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeMenu aData aMode dt a = a
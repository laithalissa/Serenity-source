{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Menu where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External

import Control.Lens

data MenuData a = MenuData
	{	_menuTitleLabel    :: Label a
	,	_menuVersionLabel  :: Label a
	,	_menuHostButton    :: Button a ApplicationMode
	,	_menuJoinButton    :: Button a ApplicationMode
	,	_menuDemoButton    :: Button a ApplicationMode
	,	_menuCreditsButton :: Button a ApplicationMode
	,	_menuQuitButton    :: Button a ApplicationMode
	}
makeLenses ''MenuData

class AppState a => MenuState a where
	aMenu :: Simple Lens a (MenuData a)

initMenuData :: MenuState a => Assets -> MenuData a
initMenuData assets = MenuData
	{	_menuTitleLabel    = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_menuVersionLabel  = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_menuHostButton    = initMenuButton "Host      ->" (\_ -> Host)
	,	_menuJoinButton    = initMenuButton "Join      ->" (\_ -> Join)
	,	_menuDemoButton    = initMenuButton "Quick     ->"  (\_ -> Quick)
	,	_menuCreditsButton = initMenuButton "Credits    "  (\_ -> Credits)
	,	_menuQuitButton    = initMenuButton "Quit      -<" (\_ -> Quit)
	}

viewMenu :: MenuState a => a -> View a
viewMenu a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	label a (aMenu.menuTitleLabel) ((30,650),(220,30))
	,	label a (aMenu.menuVersionLabel) ((0,0),(100,15))
	,	(initBox ((680, 0), (345, 750))) <++
		[	button a (aMenu.menuHostButton)    aMode ((80,650),(185,28))
		,	button a (aMenu.menuJoinButton)    aMode ((80,550),(185,28))
		,	button a (aMenu.menuDemoButton)    aMode ((80,450),(185,28))
		,	button a (aMenu.menuCreditsButton) aMode ((80,150),(185,28))
		,	button a (aMenu.menuQuitButton)    aMode ((80, 50),(185,28))
		]
	]

timeMenu :: MenuState a => Float -> a -> a
timeMenu dt a = a
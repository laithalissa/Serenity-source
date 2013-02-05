{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

gui = do
	--assets <- Assets.initialize
	playIOZero
		(InWindow "Project Serenity" (1024, 768) (0, 0))
		black
		30
		(SerenityApplicationController ["Menu 1", "Menu 2"] 0 (Label "This is a label" black (Just red) 1))
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		(\_ -> \a -> return a)

data SerenityApplicationController = SerenityApplicationController
	{	_appMenus :: [String]
	,	_appCount :: Int
	,	_appLabel :: Label
	}
makeLenses ''SerenityApplicationController

instance ViewController SerenityApplicationController where
	getView app = 
		(initView ((0, 0), (1024, 768)))
		<++ 
		[	label app appLabel ((100,100),(16,110))
		,	label app appLabel ((100,400),(16,110))
		]

{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.UI.Main
(	gui
) where

import Serenity.Sheen

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Control.Lens

gui = do
	--assets <- Assets.initialize
	playIOZero
		(InWindow "Project Serenity" (1024, 768) (0, 0))
		black
		30
		(SerenityApplication ["Menu 1", "Menu 2"] 0)
		(\a -> return $ draw a)
		(\event -> \a -> return $ handleEvent event a)
		(\_ -> \a -> return a)

data SerenityApplication = SerenityApplication
	{	_appMenus :: [String]
	,	_appCount :: Int
	}
makeLenses ''SerenityApplication

instance ViewController SerenityApplication where
	getView = (initView (0, 1024, 0, 768))
		{	_viewDepict = Just $ \app -> Color green $ Text $ (app^.appMenus) !! 0
		,	_viewBackground = Just black
		}

--makeMenus = 
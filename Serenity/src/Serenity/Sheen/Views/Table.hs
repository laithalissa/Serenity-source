{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.Table where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

data Table a = Table
	{	_tableCellSep    :: Int
	,	_tableBackground :: Maybe Color
	}
makeLenses ''Table

initTable :: Int -> Maybe Color -> Table a
initTable sep backg = Table
	{	_tableCellSep = sep
	,	_tableBackground = backg
	}

table :: a -> Simple Lens a (Table a) -> Getter a [b] -> (b -> View a) -> ((Int, Int), (Int, Int)) -> View a
table a table bsLens b2View bounds = 
	(initView bounds) & (viewBackground .~ (a^.table.tableBackground)) 
	<++ map (\(b, i) -> (viewOrigin._2 .~ top - sep*i) $ b2View b) ((a^.bsLens) `zip` [1..])
	where 
		sep = a^.table.tableCellSep
		top = (bounds^._1._2) + (bounds^._2._2)

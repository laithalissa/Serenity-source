{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.Table where

import Serenity.Sheen.View

import Graphics.Gloss.Interface.IO.Game
import Control.Lens

data Table a = Table
	{	_tableNumCols    :: Int
	,	_tableCellSep    :: Int
	,	_tableBackground :: Maybe Color
	}
makeLenses ''Table

initTable :: Int -> Int -> Maybe Color -> Table a
initTable cols sep backg = Table
	{	_tableNumCols = cols
	,	_tableCellSep = sep
	,	_tableBackground = backg
	}

table :: a -> Simple Lens a (Table a) -> Getter a [b] -> (b -> View a) -> ((Int, Int), (Int, Int)) -> View a
table a table bsLens b2View bounds = 
	(initView bounds) & (viewBackground .~ (a^.table.tableBackground)) 
	<++ map (\(b, i) -> (viewOrigin .~ (0 + sep * (i `mod` cols), top - sep * (i `div` cols + 1))) $ b2View b) ((a^.bsLens) `zip` [0..])
	where 
		cols = a^.table.tableNumCols
		sep = a^.table.tableCellSep
		top = (bounds^._1._2) + (bounds^._2._2)

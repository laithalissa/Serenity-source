{-# LANGUAGE TemplateHaskell #-}

module Serenity.Model.Formation where

import Control.Lens

data Formation = Formation
	{	_formationUnits  :: [Int]
	,	_formationLeader :: Int
	,	_formationAlpha  :: Int -- ^ Distance between units in a line
	,	_formationBeta   :: Int -- ^ Distance between lines
	,	_formationGamma  :: Int -- ^ Vertical distance between units in a line
	,	_formationPsi    :: Int -- ^ Number of units per line
	}
	deriving Show
makeLenses ''Formation

-- | Give the list of unit positions from the specified set of formation parameters
formation
	:: Int -- ^ Number of units in the formation
	-> Int -- ^ Distance between units in a line
	-> Int -- ^ Distance between lines
	-> Int -- ^ Vertical distance btween units in a line
	-> Int -- ^ Number of units in a line
	-> (Double, Double) -- ^ Position of the leader
	-> [(Double, Double)]
formation n alpha beta gamma psi (x, y) = map position $ take n [0..]
	where
		position :: Int -> (Double, Double)
		position i = (xPos, yPos)
			where
				xPos
					| pos <= half = x - (fromIntegral $ alpha * pos)
					| otherwise = x + (fromIntegral $ alpha * (pos - half))
				yPos
					| pos <= half = y - (fromIntegral $ beta * line) - (fromIntegral $ gamma * pos)
					| otherwise = y - (fromIntegral $ beta * line) - (fromIntegral $ gamma * (pos - half))

				(line, pos) = divMod i psi
				half = psi `div` 2

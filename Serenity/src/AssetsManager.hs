{-# LANGUAGE TemplateHaskell #-}

module AssetsManager where

-- standard modules
import qualified Data.Map as Map

-- library modules
import Control.Lens

-- serenity modules
import qualified Serenity.Model.Ship

import Graphics.Gloss.Data.Picture
	(	Picture(..)
	)

data Assets = Assets
	{	_assetsImages :: Map String Picture
	,	_assetsShipClasses :: Map String ShipClass
	,	_assetsWeapons :: Map String Weapon
	,	_assetsSystems :: Map String System
	}
	deriving (Show, Eq)
makeLenses ''Assets


initAssets :: IO (Either String Assets)
initAssets = return $ Left "not implemented"
{-# LANGUAGE TemplateHaskell #-}

module AssetsManager where

-- standard modules
import Data.Map(Map)
import qualified Data.Map as Map

-- library modules
import Control.Lens
import Data.Yaml.YamlLight
import Paths_Serenity(getDataFileName)
import System.EasyFile(getDirectoryContents)

-- serenity modules
import Serenity.Model.Ship

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


initAssets :: FilePath -> IO (Either String Assets)
initAssets addonsDir = do
	files <- getDirectoryContents (addonsDir ++ "/ships")
	return $ Left $ show $ files


	-- path <- getDataFileName "templates/ships/destroyer.yml"
	-- return $ Left $ show $ path
	-- node <- parseYamlFile "templates/ships/destroyer.yml"
	-- return $ Left $ show node
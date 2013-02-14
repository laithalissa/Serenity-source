{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows, TypeFamilies #-}

module Serenity.External.Common where


import Control.Applicative(liftA)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Prelude hiding(id, (.))
import Data.Set(Set)
import qualified Data.Set as Set

import Control.Arrow
import Control.Category
import Control.Lens
import Data.ByteString.Char8(ByteString, pack, unpack)
import Data.Yaml.YamlLight
import System.EasyFile(getDirectoryContents, pathSeparator, splitFileName, dropExtensions, takeExtensions, getCurrentDirectory)


defaultAssetsDirectory :: IO FilePath
defaultAssetsDirectory = do
	dir <- getCurrentDirectory 
	return $ foldl1 subdir [dir, "resources", "templates"]

assetsDirectory' :: [FilePath] -> IO FilePath
assetsDirectory' dirs = do
	defaultDir <- defaultAssetsDirectory
	let directory = foldl subdir defaultDir dirs
	return directory

getDirectoryFiles 
	:: (FilePath, String)  -- ^ directory, and file extension to filter the files against, extension must contain the dot
	-> IO [FilePath] -- ^ all files in the directory which have the specified file extension
getDirectoryFiles = proc (folder, extension) -> do
	contents <- getDirectoryContents -< folder
	cleanedContents <- (liftA (uncurry . clean)) -< (extension, contents)
	mapF <- (\folder child -> subdir folder child) -< folder
	qualifiedContents <- (liftA $ uncurry map) -< (mapF, cleanedContents)
	--qualifiedContents <- (liftA $ map (\f->subdir folder f)) -< cleanedContent
	returnA -< qualifiedContents
		where
		clean :: String -> [FilePath] -> [FilePath]
		clean extension = filter ((==) extension . takeExtensions) 


subdir :: FilePath -> FilePath -> FilePath
subdir parent child = parent ++ (pathSeparator : child)
	

data YamlForm a = YamlForm
	{	_yamlFormToYaml :: (a, String, String) -> Yaml -- ^ takes an instance, name, and asset name and produces a yaml node
	,	_yamlFormFromYaml :: Yaml -> a
	,	_yamlFormName :: Yaml -> String
	,	_yamlFormAsset :: Yaml -> String
	,	_yamlFolder :: String
	}
	deriving(Show, Eq)
$( makeLenses [''YamlForm] )

-- simplified version of YamlLight, changes: no bytestring, keys to maps are strings
data Yaml = 
	  YamlMap { yamlMap :: (Map String Yaml) }
	| YamlList { yamlList :: [Yaml] }
	| YamlString { yamlString :: String }
	| YamlNull 
	deriving (Show, Eq)

yamlLookup :: String -> Yaml -> Yaml
yamlLookup key (YamlMap mapping) = fromJust $ Map.lookup key mapping

yamlLookupString :: String -> Yaml -> String
yamlLookupString key node = yamlString $ yamlLookup key node

loadYamlForm ::  YamlForm a -> IO [Yaml]
loadYamlForm yamlForm = do
	generalDirectory <- defaultAssetsDirectory
	directory <- subdir generalDirectory (yamlForm^.yamlFolder)
	fileNames <- getDirectoryFiles (directory, ".yml")
	loadYamlNode' fileNames


conversion :: YamlLight -> Yaml
conversion YNil = YamlNull
conversion (YStr bs) = YamlString $ unpack bs
conversion (YSeq xs) = YamlList (map conversion xs)
conversion (YMap mapping) = YamlMap $ Map.fromList $ map f (Map.toList mapping)
	where f ((YStr k), val) = (unpack k, conversion val)

loadYamlNode' :: [FilePath] -> IO [Yaml]
loadYamlNode' fileNames = sequence $ map loadYamlNode fileNames

loadYamlNode :: FilePath -> IO Yaml
loadYamlNode fileName = liftA conversion $ parseYamlFile fileName

assemble :: (Yaml -> a) -> FilePath -> IO a
assemble f file = do
	node <- parseYamlFile file
	let yamlRoot = conversion node
	return $ f yamlRoot
	



-- lenses

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module AssetsManager where
-- (	Assets(..)
-- ,	Addons(..)
-- ,	initAddons
-- ,	initAssets
-- ,	getPicture
-- ,	sizeTo
-- ,	getPictureSized
-- ) where

import Serenity.Debug(trace', traceShow, trace)

-- standard modules
import Control.Applicative(liftA)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Prelude hiding(id, (.))
import Data.Set(Set)
import qualified Data.Set as Set
import Text.Printf(printf)

-- library modules
import Control.Arrow
import Control.Category
import Control.Lens
import Data.ByteString.Char8(ByteString, pack, unpack)
import Data.Yaml.YamlLight
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Paths_Serenity
import System.EasyFile(getDirectoryContents, pathSeparator, splitFileName, dropExtensions, takeExtensions, getCurrentDirectory)

-- serenity modules
import Serenity.Model.Sector(Resources(..))

import Serenity.Model.Ship


type ShipFile = String
type SystemFile = String
type TextureFile = String
type WeaponFile = String

shipClassesDirName = ("ships", ".yml")
systemsDirName = ("systems", ".yml")
weaponsDirName = ("weapons", ".yml")
texturesDirName = ("textures", ".bmp")

-- | gets all the relevent asset files from the directory passed
getFileNames :: FilePath -> IO ([ShipFile], [WeaponFile], [SystemFile], [TextureFile])
getFileNames dir = do
	shipFiles <- getDirectoryFiles shipClassesDirName
	weaponFiles <- getDirectoryFiles weaponsDirName
	systemFiles <- getDirectoryFiles systemsDirName
	textureFiles <- getDirectoryFiles texturesDirName
	return (shipFiles, weaponFiles, systemFiles, textureFiles)

		where
		getDirectoryFiles :: (FilePath, String) -> IO [FilePath]
		getDirectoryFiles = proc (child, extension) -> do
			folder <- (uncurry subdir) -< (dir, child)
			contents <- getDirectoryContents -< folder
			returnA -< liftA (map (subdir dir . subdir child) . clean extension) contents

		clean :: String -> [FilePath] -> [FilePath]
		clean extension = filter ((==) extension . takeExtensions) 

subdir :: FilePath -> FilePath -> FilePath
subdir parent child = parent ++ (pathSeparator : child)
	

loadImages :: [FilePath] -> IO (Map FilePath Picture)
loadImages files = liftA Map.fromList $ sequence $ map fileF files
	where
	fileF :: FilePath -> IO (FilePath, Picture)
	fileF fileName = do
		let name = snd $ splitFileName fileName
		image <- loadBMP fileName
		return (name, image)


data Addons = Addons
	{	_addonsShipClasses :: Map String ShipClass
	,	_addonsWeapons :: Map String Weapon
	,	_addonsSystems :: Map String System
	} deriving(Show, Eq)
makeLenses ''Addons


data Assets = Assets
	{	_assetsShipClasses :: Map String Picture
	,	_assetsWeapons :: Map String Picture
	,	_assetsSystems :: Map String Picture
	,	_assetsTextures :: Map String Picture -- ^ legacy
	}
	deriving (Show, Eq)
makeLenses ''Assets

data Bundle = Bundle
	{	_bundleShipClasses :: Map String (ShipClass, Picture)
	,	_bundleWeapons :: Map String (Weapon, Picture)
	,	_bundleSystems :: Map String (System, Picture)
	,	_bundleTextures :: Map String Picture
	}
	deriving (Show, Eq)
makeLenses ''Bundle

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
---------- exported functions ----------

initAddons :: FilePath -> IO Addons
initAddons addonsDir = 	do
	bundle <- initBundle addonsDir
	let shipClasses = Map.map fst $ (bundle^.bundleShipClasses)
	let weapons = Map.map fst $ (bundle^.bundleWeapons)
	let systems = Map.map fst $ (bundle^.bundleSystems)
	return $ Addons shipClasses weapons systems 

initAssets :: FilePath -> IO Assets
initAssets addonsDir = do
	bundle <- initBundle addonsDir
	let shipClasses = Map.map snd $ (bundle^.bundleShipClasses)
	let weapons = Map.map snd $ (bundle^.bundleWeapons)
	let systems = Map.map snd $ (bundle^.bundleSystems)
	let textures = (bundle^.bundleTextures)
	return $ Assets shipClasses weapons systems textures

defaultAssetsDirectory = do
	dataDir <- getDataDir
	return $ subdir dataDir "templates"

emptyAddons :: Addons
emptyAddons = Addons Map.empty Map.empty Map.empty

emptyAssets :: Assets
emptyAssets = Assets Map.empty Map.empty Map.empty Map.empty

getPictureSized :: String -> Float -> Float -> Assets -> Picture
getPictureSized name nWidth nHeight assets = sizeTo nWidth nHeight (getPicture name assets)

sizeTo :: Float -> Float -> Picture -> Picture
sizeTo nWidth nHeight Blank                             = Blank
sizeTo nWidth nHeight (Translate width height subImage) = translate width height $ sizeTo nWidth nHeight subImage
sizeTo nWidth nHeight (Scale scaleX scaleY subImage)    = scale scaleX scaleY $ sizeTo (nWidth/scaleX) (nHeight/scaleY) subImage
sizeTo nWidth nHeight (Rotate rotation subImage)        = rotate rotation $ sizeTo nWidth nHeight subImage
sizeTo nWidth nHeight (Pictures subImages)              = pictures $ map (sizeTo nWidth nHeight) subImages
sizeTo nWidth nHeight image@(Bitmap width height _ _)   = scale s s image 
	where
	s = (max nWidth nHeight) / (fromIntegral $ max width height)

getPicture :: String -> Assets -> Picture
getPicture name assets = case (Map.lookup name $ assets^.assetsTextures) of
	Just asset -> asset
	Nothing -> color red $ text ("Couldn't load asset " ++ name)

---------- end of helper functions ----------

initBundle :: FilePath -> IO Bundle
initBundle addonsDir = do
	files@(shipClassFiles, weaponFiles, systemFiles, textureFiles) <- getFileNames addonsDir
	imageMap <- loadImages textureFiles
	let load' = load imageMap
	shipClasses <- load' shipClassMaker shipClassFiles
	weapons <- load' weaponMaker weaponFiles
	systems <- load' systemMaker systemFiles
	textures <- return $ Map.fromList $ map (\(k,v)->(dropExtensions k,v)) $ Map.toList imageMap
	return $ Bundle shipClasses weapons systems textures
	
	where
	load :: Map FilePath Picture -> (Yaml -> (a, String, FilePath)) -> [FilePath] -> IO ( Map String (a, Picture) )
	load imageMapping maker files = do
		result <- sequence $ map f' files
		return $ Map.fromList result
		where 
		f (thing, name, fileName) = (name, (thing, fromJust $ Map.lookup fileName imageMapping))
		f' file = do
			result <- assemble maker file
			return $ f result

conversion :: YamlLight -> Yaml
conversion YNil = YamlNull
conversion (YStr bs) = YamlString $ unpack bs
conversion (YSeq xs) = YamlList (map conversion xs)
conversion (YMap mapping) = YamlMap $ Map.fromList $ map f (Map.toList mapping)
	where f ((YStr k), val) = (unpack k, conversion val)

assemble :: (Yaml -> a) -> FilePath -> IO a
assemble f file = do
	node <- parseYamlFile file
	let yamlRoot = conversion node
	return $ f yamlRoot

---------- Ship Class ----------
shipClassMaker :: Yaml -> (ShipClass, String, FilePath)
shipClassMaker node = (ShipClass cor' strength' weapons' systems', name', imageName')
	where
		name' = yamlLookupString "shipName" node
		imageName' = yamlLookupString "fileName" node
		cor' = read $ yamlLookupString "centerOfRotation" node
		strength' = damageStrengthMaker $ yamlLookup "damageStrength" node
		weapons' = map weaponSlotMaker $ yamlList $ yamlLookup "weaponSlots" node
		systems' = map systemSlotMaker $  yamlList $ yamlLookup "systemSlots" node

damageStrengthMaker :: Yaml -> Damage
damageStrengthMaker node = (Damage hull' shields')
	where
	hull' = read $ yamlLookupString "hull" node
	shields' = read $ yamlLookupString "shields" node

weaponSlotMaker :: Yaml -> WeaponSlot
weaponSlotMaker node = WeaponSlot location' direction' type'
	where
		location' = read $ yamlLookupString "location" node
		direction' = read $ yamlLookupString "direction" node
		type' = read $ yamlLookupString "type" node

systemSlotMaker :: Yaml -> SystemSlot
systemSlotMaker node = SystemSlot location' direction'
	where
		location' = read $ yamlLookupString "location" node
		direction' = read $ yamlLookupString "direction" node

---------- Weapon ----------
weaponMaker 
	:: Yaml -- ^ yaml node 
	-> ( Weapon -- ^ weapon extracted from node 
	   , String -- ^ weapon name
	   , FilePath -- ^ weapon image file
	   )
weaponMaker node = (weapon, name, fileName)
	where
		weapon = Weapon range' effect' reloadTime' accuracy' cost'
		name = yamlLookupString "weaponName" node
		fileName = yamlLookupString "fileName" node
		range' = read $ yamlLookupString "range" node
		reloadTime' = read $ yamlLookupString "reloadTime" node
		accuracy' = read $ yamlLookupString "accuracy" node
		effect' = weaponDamageMaker $ yamlLookup "damage" node
		cost' = weaponUseCostMaker $ yamlLookup "useCost" node
		

weaponDamageMaker 
	:: Yaml -- ^ yaml node
	-> WeaponEffect -- ^ weapon damage extracted from node
weaponDamageMaker node = WeaponEffect shield' hull' penetration'
	where
		shield' = read $ yamlLookupString "shield" node
		hull' = read $ yamlLookupString "hull" node
		penetration' = read $ yamlLookupString "penetration" node


weaponUseCostMaker 
	:: Yaml -- ^ yaml node
	-> Resources -- ^ resource cost extracted from node
weaponUseCostMaker node = Resources fuel' metal' antimatter'
	where
		fuel' = read $ yamlLookupString "fuel" node
		metal' = read $ yamlLookupString "metal" node
		antimatter' = read $ yamlLookupString "antiMatter" node


---------- Systems ----------

systemMaker
	:: Yaml -- ^ yaml node
	-> ( System -- ^ ship system extracted from node
	   , String -- ^ system name
	   , FilePath -- ^ filename of system image
	   ) 
systemMaker node = (System shield' hull' engine', name', fileName')
	where
		name' = yamlLookupString "name" node
		fileName' = yamlLookupString "fileName" node
		shield' = read $ yamlLookupString "shield" node
		hull' = read $ yamlLookupString "hull" node
		engine' = read $ yamlLookupString "speed" node



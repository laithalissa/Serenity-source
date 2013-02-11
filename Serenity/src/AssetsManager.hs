{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module AssetsManager where

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
import Paths_Serenity(getDataFileName)
import System.EasyFile(getDirectoryContents, pathSeparator, splitFileName, takeExtensions)

-- serenity modules
import Serenity.Model.Sector(Resources(..))

import Serenity.Model.Ship
	(	WeaponSlot(..)
	,	SystemSlot(..)
	,	ShipClass(..)
	,	Weapon(..)
	,	WeaponEffect(..)
	,	System(..)
	)


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

initAssets :: FilePath -> IO Assets
initAssets addonsDir = do
	files@(shipClassFiles, weaponFiles, systemFiles, textureFiles) <- getFileNames addonsDir
	imageMap <- loadImages textureFiles
	let load' = load imageMap
	shipClasses <- load' shipClassMaker
	weapons <- load' weaponMaker
	systems <- load' systemMaker
	print shipClasses
	print weapons
	print systems
	return NoAssets

	
		where
		load :: Map FilePath Picture -> (Yaml -> a) -> [FilePath] -> IO ( Map String (a, Picture) )
		load imageMapping maker files = liftA Map.fromList $ sequence $ map (liftA f . assemble maker) files
			where f (thing, name, fileName) = (thing, name, fromJust $ Map.lookup fileName imageMapping)

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
shipClassMaker node = (ShipClass cor' weapons' systems', name', imageName')
	where
		name' = yamlLookupString "shipName" node
		imageName' = yamlLookupString "fileName" node
		cor' = read $ yamlLookupString "centerOfRotation" node
		weapons' = map weaponSlotMaker $ yamlList $ yamlLookup "weaponSlots" node
		systems' = map systemSlotMaker $  yamlList $ yamlLookup "systemSlots" node

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



---------- End ----------



data Assets = NoAssets | Assets
	{	_assetsShipClasses :: Map String (ShipClass, Picture)
	,	_assetsWeapons :: Map String (Weapon, Picture)
	,	_assetsSystems :: Map String (System, Picture)
	}
	deriving (Show, Eq)
makeLenses ''Assets


loadImages :: [FilePath] -> IO (Map FilePath Picture)
loadImages files = liftA Map.fromList $ sequence $ map fileF files
	where
	fileF :: FilePath -> IO (FilePath, Picture)
	fileF fileName = do
		let name = snd $ splitFileName fileName
		image <- loadBMP fileName
		return (name, image)


	



-- old --

-- -- addons subdirectories and extension --


-- -- ship class fields --
-- shipClassFields = Set.fromList
-- 	[	shipClassName
-- 	,	shipClassFileName
-- 	,	shipClassCenterOfRotation
-- 	,	shipClassWeaponSlots
-- 	,	shipClassSystemSlots
-- 	]

-- shipClassFileName = "fileName"
-- shipClassName = "shipName"
-- shipClassCenterOfRotation = "centerOfRotation"
-- shipClassWeaponSlots = "weaponSlots"
-- shipClassSystemSlots = "systemSlots"

-- shipClassWeaponSlotLocation = "location"
-- shipClassWeaponSlotDirection = "direction"
-- shipClassWeaponSlotType = "type"

-- shipClassSystemSlotLocation = "location"
-- shipClassSystemSlotDirection = "direction"








-- -- ship class --
-- loadShipClass :: Map FilePath Picture -> YamlLight -> Either String (ShipClass, Picture)
-- loadShipClass images mapping = do
-- 		YStr shipName <- lookup'' shipClassName mapping
-- 		YStr fileName <- lookup'' shipClassFileName mapping
-- 		image <- lookup'' fileName images
-- 		YStr centerOfRotation <- lookup'' shipClassCenterOfRotation mapping
-- 		YSeq weaponSlotNodes <- lookup'' shipClassWeaponSlots mapping
-- 		YSeq systemSlotNodes <- lookup'' shipClassSystemSlots mapping
-- 		weaponSlots <- sequence $ map loadWeaponSlot weaponSlotNodes 
-- 		systemSlots <- sequence $ map loadSystemSlot systemSlotNodes
-- 		return $ (ShipClass (unpack shipName) (read $ unpack centerOfRotation) weaponSlots systemSlots, image)
-- 			where
-- 			lookup'' = lookup' "ShipClass"
		
-- loadShipClass _ _ = Left "invalid ship class fields"	

-- loadWeaponSlot :: YamlLight -> Either String WeaponSlot
-- loadWeaponSlot mapping = mte "failed to load weapon slots" $ do
-- 	YStr location <- lookup' shipClassWeaponSlotLocation mapping
-- 	YStr direction <- lookup' shipClassWeaponSlotDirection mapping
-- 	YStr slotType <- lookup' shipClassWeaponSlotType mapping
-- 	return $ WeaponSlot (read $ unpack location) (read $ unpack direction) (read $ unpack slotType)
-- loadWeaponSlot _ = Left "failed to load weapon slots"


-- loadSystemSlot :: YamlLight -> Either String SystemSlot
-- loadSystemSlot mapping = mte "failed to load system slot" $ do
-- 	YStr location <- lookup' shipClassSystemSlotLocation mapping
-- 	YStr direction <- lookup' shipClassSystemSlotDirection mapping
-- 	return $ SystemSlot (read $ unpack location) (read $ unpack direction)
-- loadSystemSlot _ = Left "failed to load system slot"


-- -- weapon --

-- fieldWeaponName = "weaponName"
-- fieldWeaponFileName = "fileName"
-- fieldWeaponRange = "range"
-- fieldWeaponReloadTime = "reloadTime"
-- fieldWeaonAccuracy = "accuracy"
-- fieldWeaponDamage = "damage"
-- fieldWeaponUseCost = "useCost"

-- fieldWeaponDamageShield = "shield"
-- fieldWeaponDamageHull = "hull"
-- fieldWeaponDamagePenetration = "penetration"

-- fieldWeaponUseCostFuel = "fuel"
-- fieldWeaponUseCostMetal = "metal"
-- fieldWeaponUseCostAntiMatter = "antiMatter"

-- loadWeapon :: Map FilePath Picture -> YamlLight -> Either String (Weapon, Picture)
-- loadWeapon imageMapping mapping = do
-- 	YStr weaponName <- lookup'' fieldWeaponName mapping
-- 	YStr fileName <- lookup'' fieldWeaponFileName mapping
-- 	image <- Map.lookup (unpack fileName) imageMapping
-- 	YStr range <- lookup'' fieldWeaponRange mapping
-- 	YStr reloadTime <- lookup'' fieldWeaponReloadTime mapping
-- 	YStr accuracy <- lookup'' fieldWeaponAccuracy mapping
-- 	damageNode <- lookup'' fieldWeaponDamage mapping
-- 	useCostNode <- lookup'' fieldWeaponUseCost mapping
-- 	damage <- loadWeaponDamage damageNode
-- 	useCost <- loadWeaponCost useCostNode
-- 	let weapon = Weapon
-- 		{	_weaponRange=(unpack' range)
-- 		,	_weaponEffect=damage
-- 		,	_weaponReloadTime=(unpack' reload)
-- 		,	_weaponAccuracy=(unpack' accuracy)
-- 		,	_weaponFiringCost=useCost
-- 		}
-- 	return $ (weapon, image)
-- 		where
-- 		lookup'' = lookup' "Weapon"

-- loadWeaponDamage :: YamlLight -> Either String WeaponEffect
-- loadWeaponDamage mapping = do
-- 	YStr shield <- lookup'' fieldWeaponDamageShield mapping
-- 	YStr hull <- lookup'' fieldWeaponDamageHull mapping
-- 	YStr penetration <- lookup'' fieldWeaponDamagePenetration mapping
-- 	return $ WeaponEffect (unpack' shield) (unpack' hull) (unpack' penetration)
-- 		where
-- 		lookup'' = lookup' "WeaponDamage"

-- loadWeaponCost :: YamlLight -> Either String Resources
-- loadWeaponCost mapping = do
-- 	YStr fuel <- lookup'' fieldWeaponUseCostFuel mapping
-- 	YStr metal <- lookup'' fieldWeaponUseCostMetal mapping
-- 	YStr antiMatter <- lookup'' fieldWeaponUseCostAntimatter mapping
-- 	return $ Resources (unpack' fuel) (unpack' metal) (unpack' antiMatter)
-- 		where
-- 		lookup'' = lookup' "WeaponUseCost"

-- -- helpers --

-- type Loader a = (Map FilePath Picture -> YamlLight -> Either String (a, Picture))

-- load :: Map String Picture -> Loader a -> [FilePath] -> IO (Either String [(a, Picture)])
-- load imageMapping loader files = do
-- 	inside <- ioList
-- 	return $ sequence inside
-- 		where 
-- 		ioList :: IO [Either String (a, Picture)]
-- 		ioList = sequence $ map f files

-- 		f :: FilePath -> IO (Either String (a, Picture))
-- 		f = liftA (loader imageMapping) . parseYamlFile

-- unpack' :: ByteString -> a
-- unpack' = read . unpack

-- lookup' :: String -> String -> YamlLight -> Either String YamlLight
-- lookup' msg key (YMap mapping) = case (Map.lookup (YStr $ pack key) mapping) of
-- 	Just result -> Right result
-- 	Nothing -> Left $ printf "error loading %s: %s" msg key

-- mte :: a -> Maybe b -> Either a b
-- mte a (Just b) = Right b
-- mte a Nothing = Left a





-- 	-- path <- getDataFileName "templates/ships/destroyer.yml"
-- 	-- return $ Left $ show $ path
-- 	-- node <- parseYamlFile "templates/ships/destroyer.yml"
-- 	-- return $ Left $ show node
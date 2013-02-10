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
import Serenity.Model.Ship
	(	WeaponSlot(..)
	,	SystemSlot(..)
	,	ShipClass(..)
	,	Weapon(..)
	,	System(..)
	)


-- description of yaml structure

data Description = 
	  DescriptionRoot { isList :: Bool, descriptionFields :: [Description] }
	| DescriptionNamespace { descriptionName :: String, isList :: Bool, descriptionFields :: [Description] }
	| DescriptionField { descriptionName :: String }
	deriving(Show, Eq)

makeSimpleNamespace :: String -> Bool -> [String] -> Description
makeSimpleNamespace name isList fieldNames = makeNamespace name isList fieldNames []

makeNamespace :: String -> Bool -> [String] -> [Description] -> Description
makeNamespace name = _makeDescriptionAbstract (DescriptionNamespace name)

makeDescription :: [String] -> [Description] -> Description
makeDescription = _makeDescriptionAbstract DescriptionRoot False

_makeDescriptionAbstract :: (Bool -> [Description] -> Description) -> Bool -> [String] -> [Description] -> Description
_makeDescriptionAbstract f isList fieldNames nestedFields = f isList $ (map DescriptionField fieldNames) ++ nestedFields

descriptionRootToNamespace :: String -> Description -> Description
descriptionRootToNamespace name (DescriptionRoot isList fields) = DescriptionNamespace name isList fields	

-- parserTree

data ParseTree =
	  ParseRoot { parseFields :: [ParseTree] }
	| ParseNamespace { parseName :: String, parseFields :: [ParseTree] }
	| ParseField { parseName :: String, parseValue :: String }
	deriving (Eq)

instance Show ParseTree where
	show (ParseRoot fields) = foldl1 (++) $ map (unlines . map ("    "++) . lines . show) fields
	show (ParseNamespace name fields) = (++) (name ++ ":\n")  $ foldl1 (++) $ map (unlines . map ("    "++) . lines . show) fields
	show (ParseField name val) = printf "%s: %s\n" name val

assemble :: (ParseTree -> a) -> Description -> FilePath -> IO a
assemble f description file = do
	node <- parseYamlFile file
	let tree = (assembleNode node description) !! 0
	return $ f tree
		where
		assembleNode :: YamlLight -> Description -> [ParseTree]
		assembleNode (YStr value) (DescriptionField name) = [ParseField name (unpack value)]
		assembleNode (YMap mapping) (DescriptionRoot False fields) = [ParseRoot  fields']
			where
			fields' = concat $ map fieldsF fields
			fieldsF field = assembleNode node field
				where
				node = fromJust $ Map.lookup (YStr $ pack $ descriptionName field) mapping

		assembleNode (YMap mapping) (DescriptionNamespace name False fields) = [ParseNamespace name fields']
			where
			fields' = concat $ map fieldsF fields
			fieldsF field = assembleNode node field
				where
				node = fromJust $ Map.lookup (YStr $ pack $ descriptionName field) mapping

		assembleNode (YSeq nodeList) desc@(DescriptionNamespace name True fields) = [ParseNamespace name fields']
			where
			fields' = concat $ map (\n -> assembleNode n desc{isList=False}) nodeList

		assembleNode node description = trace (printf "node='%s', description='%s'" (show node) (show description)) (error "failed :(")
			
			


shipClassDescription :: Description 
shipClassDescription = makeDescription 
	[	"shipName"
	,	"fileName"
	,	"centerOfRotation"
	]
	[	makeSimpleNamespace "weaponSlots" True ["location", "direction", "type"]
	,	makeSimpleNamespace "systemSlots" True ["location", "direction"]
	]


data Assets = NoAssets | Assets
	{	_assetsShipClasses :: Map String (ShipClass, Picture)
	,	_assetsWeapons :: Map String (Weapon, Picture)
	,	_assetsSystems :: Map String (System, Picture)
	}
	deriving (Show, Eq)
makeLenses ''Assets


initAssets :: FilePath -> IO Assets
initAssets addonsDir = do
	files@(shipClassFiles, weaponFiles, systemFiles, textureFiles) <- getFileNames addonsDir
	shipClass <- assemble loadShipClass shipClassDescription (shipClassFiles !! 0)
	print shipClass
	return NoAssets

loadShipClass :: ParseTree -> (ShipClass, FilePath)
loadShipClass tree = (ShipClass name' cor' weaponSlots' systemSlots', fileName')
	where
	name' = readString tree "shipName"
	fileName' = readString tree "fileName"
	cor' = readType tree "centerOfRotation"
	weaponSlots' = map loadWeaponSlot $ lookupT tree "weaponSlots"
	systemSlots' = map loadSystemSlot $ lookupT tree "systemSlots"
	

loadWeaponSlot :: ParseTree -> WeaponSlot
loadWeaponSlot tree = WeaponSlot location' direction' type'
	where
	location' = readType tree "location"
	direction' = readType tree "direction"
	type' = readType tree "type"

loadSystemSlot :: ParseTree -> SystemSlot
loadSystemSlot tree = SystemSlot location' direction'
	where
	location' = readType tree "location"
	direction' = readType tree "direction"

readType :: (Read a) => ParseTree -> String -> a
readType tree name = read $ readString tree name

readString :: ParseTree -> String -> String
readString tree name = parseValue $ (lookupT tree name) !! 0

lookupT :: ParseTree -> String -> [ParseTree]
lookupT tree name = if results == [] then error msg else results
	where 
		msg = printf "failed to lookup key '%s' in\n'%s'\n" name (show tree)
		results = filter f (parseFields tree)
		f (ParseNamespace n fs) = name == n
		f (ParseField n v) = name == n
		f x = False
	
loadImages :: [FilePath] -> IO (Map FilePath Picture)
loadImages files = liftA Map.fromList $ sequence $ map fileF files
	where
	fileF :: FilePath -> IO (FilePath, Picture)
	fileF fileName = do
		let name = snd $ splitFileName fileName
		image <- loadBMP fileName
		return (name, image)

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

unpack' :: (Read a) => ByteString -> a
unpack' = read . unpack

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
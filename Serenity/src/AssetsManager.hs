{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module AssetsManager where

-- standard modules
import Control.Applicative(liftA)
import Data.Map(Map)
import qualified Data.Map as Map
import Prelude hiding(id, (.))
import Data.Set(Set)
import qualified Data.Set as Set
import Text.Printf(printf)

-- library modules
import Control.Arrow
import Control.Category
import Control.Lens
import Data.ByteString.Char8(pack, unpack)
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



-- addons subdirectories and extension --
shipClassesDirName = ("ships", ".yml")
systemsDirName = ("systems", ".yml")
weaponsDirName = ("weapons", ".yml")
texturesDirName = ("textures", ".bmp")

-- ship class fields --
shipClassFields = Set.fromList
	[	shipClassName
	,	shipClassFileName
	,	shipClassCenterOfRotation
	,	shipClassWeaponSlots
	,	shipClassSystemSlots
	]

shipClassFileName = "fileName"
shipClassName = "shipName"
shipClassCenterOfRotation = "centerOfRotation"
shipClassWeaponSlots = "weaponSlots"
shipClassSystemSlots = "systemSlots"

shipClassWeaponSlotLocation = "location"
shipClassWeaponSlotDirection = "direction"
shipClassWeaponSlotType = "type"

shipClassSystemSlotLocation = "location"
shipClassSystemSlotDirection = "direction"


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
	files@(shipClassFiles, weaponFiles, systemFiles, textureFiles) <- getFileNames addonsDir
	imageMapping <- loadImages textureFiles
	shipClasses <- loadShipClasses imageMapping shipClassFiles
	return $ Left $ show $ fmap (map fst) shipClasses
	
		where
		loadShipClasses :: Map String Picture -> [FilePath] -> IO (Either String [(ShipClass, Picture)])
		loadShipClasses imageMapping files = f''
			where 
				f'' :: IO (Either String [(ShipClass, Picture)])
				f'' = do
					inside <- f'
					return $ sequence inside

				f' :: IO [Either String (ShipClass, Picture)]
				f' = sequence $ map f files

				f :: FilePath -> IO (Either String (ShipClass, Picture))
				f = liftA (loadShipClass imageMapping) . parseYamlFile


loadImages :: [FilePath] -> IO (Map FilePath Picture)
loadImages files = liftA Map.fromList $ sequence $ map fileF files
	where
	fileF :: FilePath -> IO (FilePath, Picture)
	fileF fileName = do
		let name = snd $ splitFileName fileName
		image <- loadBMP fileName
		return (name, image)

-- ship class --
loadShipClass :: Map FilePath Picture -> YamlLight -> Either String (ShipClass, Picture)
loadShipClass images mapping = do
		YStr shipName <- lookup'' shipClassName mapping
		YStr fileName <- lookup'' shipClassFileName mapping
		image <- mte (msg "image") $ Map.lookup (unpack fileName) images
		YStr centerOfRotation <- lookup'' shipClassCenterOfRotation mapping
		YSeq weaponSlotNodes <- lookup'' shipClassWeaponSlots mapping
		YSeq systemSlotNodes <- lookup'' shipClassSystemSlots mapping
		weaponSlots <- sequence $ map loadWeaponSlot weaponSlotNodes 
		systemSlots <- sequence $ map loadSystemSlot systemSlotNodes
		return $ (ShipClass (unpack shipName) (read $ unpack centerOfRotation) weaponSlots systemSlots, image)
			where
			lookup'' = lookup' "ShipClass"
		
loadShipClass _ _ = Left "invalid ship class fields"	

loadWeaponSlot :: YamlLight -> Either String WeaponSlot
loadWeaponSlot mapping = mte "failed to load weapon slots" $ do
	YStr location <- lookup' shipClassWeaponSlotLocation mapping
	YStr direction <- lookup' shipClassWeaponSlotDirection mapping
	YStr slotType <- lookup' shipClassWeaponSlotType mapping
	return $ WeaponSlot (read $ unpack location) (read $ unpack direction) (read $ unpack slotType)
loadWeaponSlot _ = Left "failed to load weapon slots"


loadSystemSlot :: YamlLight -> Either String SystemSlot
loadSystemSlot mapping = mte "failed to load system slot" $ do
	YStr location <- lookup' shipClassSystemSlotLocation mapping
	YStr direction <- lookup' shipClassSystemSlotDirection mapping
	return $ SystemSlot (read $ unpack location) (read $ unpack direction)
loadSystemSlot _ = Left "failed to load system slot"


-- weapon --

fieldWeaponName = "weaponName"
fieldWeaponFileName = "fileName"
fieldWeaponRange = "range"
fieldWeaponReloadTime = "reloadTime"
fieldWeaonAccuracy = "accuracy"
fieldWeaponDamage = "damage"
fieldWeaponUseCost = "useCost"

fieldWeaponDamageShield = "shield"
fieldWeaponDamageHull = "hull"
fieldWeaponDamagePenetration = "penetration"

fieldWeaponUseCostFuel = "fuel"
fieldWeaponUseCostMetal = "metal"
fieldWeaponUseCostAntiMatter = "antiMatter"

loadWeapon :: YamlLight -> Either String Weapon
loadWeapon mapping = do
	YStr weaponName <- lookup'' fieldWeaponName mapping
	YStr fileName <- lookup'' fieldWeaponFileName mapping
	YStr range <- lookup'' fieldWeaponRange mapping
	YStr reloadTime <- lookup'' fieldWeaponReloadTime mapping
	YStr accuracy <- lookup'' fieldWeaponAccuracy mapping
	damageNode <- lookup'' fieldWeaponDamage mapping
	useCostNode <- lookup'' fieldWeaponUseCost mapping
	damage <- loadWeaponDamage damageNode
	useCost <- loadWeaponCost useCostNode
	return $ Weapon
		{	_weaponRange=(unpack' range)
		,	_weaponEffect=damage
		,	_weaponReloadTime=(unpack' reload)
		,	_weaponAccuracy=(unpack' accuracy)
		,	_weaponFiringCost=useCost
		}
		where
		lookup'' = lookup' "Weapon"


loadWeaponDamage :: YamlLight -> Either String WeaponEffect
loadWeaponDamage mapping = do
	YStr shield <- lookup'' fieldWeaponDamageShield mapping
	YStr hull <- lookup'' fieldWeaponDamageHull mapping
	YStr penetration <- lookup'' fieldWeaponDamagePenetration mapping
	return $ WeaponEffect (unpack' shield) (unpack' hull) (unpack' penetration)
		where
		lookup'' = lookup' "WeaponDamage"

loadWeaponCost :: YamlLight -> Either String Resources
loadWeaponCost mapping = do
	YStr fuel <- lookup'' fieldWeaponUseCostFuel mapping
	YStr metal <- lookup'' fieldWeaponUseCostMetal mapping
	YStr antiMatter <- lookup'' fieldWeaponUseCostAntimatter mapping
	return $ Resources (unpack' fuel) (unpack' metal) (unpack' antiMatter)
		where
		lookup'' = lookup' "WeaponUseCost"

-- helpers --

unpack' :: ByteString -> a
unpack' = read . unpack

lookup' :: String -> String -> YamlLight -> Either String YamlLight
lookup' msg key (YMap mapping) = case (Map.lookup (YStr $ pack key) mapping) of
	Just result -> Right result
	Nothing -> Left $ printf "error loading %s: %s" msg key

mte :: a -> Maybe b -> Either a b
mte a (Just b) = Right b
mte a Nothing = Left a



type ShipFile = String
type SystemFile = String
type TextureFile = String
type WeaponFile = String

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

	-- path <- getDataFileName "templates/ships/destroyer.yml"
	-- return $ Left $ show $ path
	-- node <- parseYamlFile "templates/ships/destroyer.yml"
	-- return $ Left $ show node
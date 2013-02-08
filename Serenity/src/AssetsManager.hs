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
import Data.Yaml.YamlLight
import Graphics.Gloss.Data.Picture
import Paths_Serenity(getDataFileName)
import System.EasyFile(getDirectoryContents, pathSeparator, splitFileName, takeExtensions)

-- serenity modules
import Serenity.Model.Ship
	(	WeaponSlot(..)
	,	SystemSlot(..)
	,	ShipClass(..)
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
	return $ Left $ show $ shipClasses
	
		where
		loadShipClasses :: Map String Picture -> [FilePath] -> IO [Either String ShipClass]
		loadShipClasses imageMapping = sequence . map f
			where f = liftA (loadShipClass imageMapping) . parseYamlFile


loadImages :: [FilePath] -> IO (Map String Picture)
loadImages files = sequence $ Map.fromList $ map fileF files
	where
	fileF :: FilePath -> (FilePath, Picture)
	fileF fileName = (snd $ splitFileName fileName, loadBMP fileName)

loadShipClass :: Map FilePath Picture -> YamlLight -> Either String (ShipClass, Picture)
loadShipClass images node@(YMap mapping) = do
		YStr shipName <- mte (msg "shipName") $ Map.lookup shipClassName mapping
		YStr fileName <- mte (msg "fileName") $ Map.lookup shipClassFileName mapping
		image <- mte (msg "image") $ Map.lookup fileName images
		YStr centerOfRotation <- mte (msg "centerOfRotation") $ Map.lookup shipClassCenterOfRotation mapping
		YSeq weaponSlotNodes <- mte (msg "weaponSlotNodes") $ Map.lookup shipClassWeaponSlots mapping
		YSeq systemSlotNodes <- mte (msg "systemSlotNodes") $ Map.lookup shipClassSystemSlots mapping
		weaponSlots <- sequence $ map loadWeaponSlot weaponSlotNodes 
		systemSlots <- sequence $ map loadSystemSlot systemSlotNodes
		return $ ShipClass shipName (read centerOfRotation) weaponSlots systemSlots
			where
			msg m = printf "error loading ShipClass: %s" m
		
loadShipClass _ _ = Left "invalid ship class fields"	

loadWeaponSlot :: YamlLight -> Either String WeaponSlot
loadWeaponSlot YMap mapping = mte "failed to load weapon slots" $ do
	YStr location <- Map.lookup shipClassWeaponSlotLocation mapping
	YStr direction <- Map.lookup shipClassWeaponSlotDirection mapping
	YStr slotType <- Map.lookup shipClassWeaponSlotType mapping
	return WeaponSlot (read location) (read direction) (read slotType)
loadWeaponSlot _ = Left "failed to load weapon slots"


loadSystemSlot :: YamlLight -> Either String SystemSlot
loadSystemSlot YMap mapping = mte "failed to load system slot" $ do
	YStr location <- Map.lookup shipClassSystemSlotLocation mapping
	YStr direction <- Map.lookup shipClassSystemSlotDirection mapping
	return SystemSlot (read location) (read direction)
loadSystemSlot _ = Left "failed to load system slot"



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
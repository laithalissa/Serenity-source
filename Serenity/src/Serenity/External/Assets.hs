
module Serenity.External.Assets 
(	module Serenity.External.Common 
,	initImages
,	initAddonAssets
) where

import Serenity.External.Common 

data Assets = Assets
	{	_assetsPictures :: Map String Picture
	}
	deriving (Show, Eq)
makeLenses ''Assets

initImages :: IO Assets
initImages = do
	directory <- assetsDirectory' ["textures"]
	fileNames <- getDirectoryFiles (directory, ".bmp")
	images <- loadImages fileNames
	return $ Assets images

initAddonAssets :: YamlForm a -> IO Assets
initAddonAssets yamlForm = do
	yamlNodes <- loadYamlForm yamlForm
	imageMap <- initImages 
	let yamlAssetNames = map (yamlForm^.yamlFormAsset) yamlNodes
	let yamlAssetPictures = map (\n-> fromJust $ Map.lookup n imageMap) yamlAssetNames
	return $ Assets $ Map.fromList $ zip yamlAssetNames yamlAssetPicture

loadImages :: [FilePath] -> IO (Map FilePath Picture)
loadImages files = liftA Map.fromList $ sequence $ map fileF files
	where
	fileF :: FilePath -> IO (FilePath, Picture)
	fileF fileName = do
		let name = snd $ splitFileName fileName
		image <- loadBMP fileName
		return (name, image)


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

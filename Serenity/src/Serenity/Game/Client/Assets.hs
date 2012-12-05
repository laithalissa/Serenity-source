module Serenity.Game.Client.Assets
(	Assets
,	initialize
,	getPicture
,	getPictureSized
,	sizeTo
) where

import Graphics.Gloss.Data.Color(red)

import Graphics.Gloss.Data.Picture
	(	Picture(..)
	,	color
	,	loadBMP
	,	pictures
	,	rotate
	,	scale
	,	text
	,	translate
	)

import qualified Data.Map as Map

import Paths_Serenity

data Assets = Assets
	{	textures :: Map.Map String Picture
	}
	deriving(Eq, Show)

initialize :: IO Assets
initialize = do
	planet1 <- loadAsset "planet1.bmp"
	background <- loadAsset "background.bmp"
	commander <- loadAsset "commander.bmp"
	commanderGreen <- loadAsset "commander-green.bmp"
	assets <- return $ Map.fromList
		[	("planet1", planet1)
		,	("background", background)
		,	("ship-commander", commander)
		,	("ship-commander-green", commanderGreen)
		]
	return Assets{textures=assets}

loadAsset :: FilePath -> IO Picture
loadAsset f = do
	file <- getDataFileName f
	loadBMP file

getPicture :: String -> Assets -> Picture
getPicture name assets = case (Map.lookup name $ textures assets) of
	Just asset -> asset
	Nothing -> color red $ text ("Couldn't load asset " ++ name)

getPictureSized :: String -> Float -> Float -> Assets -> Picture
getPictureSized name nWidth nHeight assets = sizeTo nWidth nHeight (getPicture name assets)

sizeTo :: Float -> Float -> Picture -> Picture
sizeTo nWidth nHeight Blank                             = Blank
sizeTo nWidth nHeight (Translate width height subImage) = translate width height $ sizeTo nWidth nHeight subImage
sizeTo nWidth nHeight (Scale scaleX scaleY subImage)    = scale scaleX scaleY $ sizeTo (nWidth/scaleX) (nHeight/scaleY) subImage
sizeTo nWidth nHeight (Rotate rotation subImage)        = rotate rotation $ sizeTo nWidth nHeight subImage
sizeTo nWidth nHeight (Pictures subImages)              = pictures $ map (sizeTo nWidth nHeight) subImages
sizeTo nWidth nHeight image@(Bitmap width height _ _)   = scale s s image where
	s = (max nWidth nHeight) / (fromIntegral $ max width height)

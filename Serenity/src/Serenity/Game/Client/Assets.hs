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
	ship1 <- loadAsset "ship1.bmp"
	ship2 <- loadAsset "ship2.bmp"
	assets <- return $ Map.fromList
		[ ("planet1", planet1)
		, ("background", background)
		, ("ship1", ship1)
		, ("ship2", ship2)
		]
	return Assets{textures=assets}

loadAsset :: FilePath -> IO Picture
loadAsset f = do
	file <- getDataFileName f
	loadBMP file

getPicture :: String -> Assets -> Picture
getPicture name assets =
	case (Map.lookup name $ textures assets) of
		Just asset -> asset
		Nothing -> color red $ text ("Couldn't load asset " ++ name)

getPictureSized :: String -> Float -> Float -> Assets -> Picture
getPictureSized name nWidth nHeight assets =
	sizeTo nWidth nHeight (getPicture name assets)


sizeTo :: Float -> Float -> Picture -> Picture
sizeTo nWidth nHeight image@(Blank) = image

sizeTo nWidth nHeight image@(Translate width height subImage) =
	translate width height $ sizeTo nWidth nHeight subImage

sizeTo nWidth nHeight image@(Scale scaleX scaleY subImage) =
	scale scaleX scaleY $ sizeTo
		(nWidth/scaleX) (nHeight/scaleY)
		subImage

sizeTo nWidth nHeight image@(Rotate rotation subImage) =
	rotate rotation $ sizeTo nWidth nHeight subImage

sizeTo nWidth nHeight image@(Pictures subImages) =
	pictures $ map (sizeTo nWidth nHeight) subImages

sizeTo nWidth nHeight image@(Bitmap width height _ _) =
	scale
	(nWidth/(fromIntegral width))
	(nHeight/(fromIntegral height))
	image

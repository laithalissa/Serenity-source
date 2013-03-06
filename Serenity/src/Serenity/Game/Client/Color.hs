module Serenity.Game.Client.Color where

import Graphics.Gloss.Data.Color
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Data.Fixed (mod')

ownerColor :: Int -> Color
ownerColor _ = red
--ownerColor ownerID = where 

convertForGloss :: RGB Float -> Color
convertForGloss rgb = makeColor8 red green blue 255 where
	red   = floor $ channelRed   rgb
	green = floor $ channelGreen rgb
	blue  = floor $ channelBlue  rgb

teamColors :: [Color]
teamColors = map (convertForGloss.rgbValue) [0..] where
	phi = 0.618033988
	hueSpace = 360

	rgbValue :: Int -> RGB Float
	rgbValue playerNumber = hsv (phiMod $ fromIntegral playerNumber) 0 0 

	--phiMod :: Int -> Int
	phiMod playerNumber = hueSpace * ((playerNumber * phi) `mod'` 1)

ownerIDColor :: Int -> Color
ownerIDColor i = teamColors !! (i+3)
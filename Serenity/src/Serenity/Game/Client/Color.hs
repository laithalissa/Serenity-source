module Serenity.Game.Client.Color where

import Graphics.Gloss.Data.Color
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Data.Fixed (mod')


ownerIDColor :: Int -> Color
ownerIDColor ownerID = teamColors !! ownerID

teamColors :: [Color]
teamColors = map (convertForGloss.rgbValue) [0..] where
	phi = 0.618033988
	hueSpace = 360

	rgbValue :: Int -> RGB Float
	rgbValue playerNumber = hsv (phiMod $ fromIntegral playerNumber) 1 0.8

	phiMod :: Int -> Float
	phiMod playerNumber = hueSpace * ((fromIntegral playerNumber * phi) `mod'` 1)

convertForGloss :: RGB Float -> Color
convertForGloss rgb = makeColor red green blue 1 where
	red   = channelRed   rgb
	green = channelGreen rgb
	blue  = channelBlue  rgb
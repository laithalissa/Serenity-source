module Serenity.Sheen.IO
(	playIOZero
) where

import Graphics.Gloss.Interface.IO.Game
import Serenity.Sheen.UIEvent (translateEvent)

playIOZero display@(InWindow _ (sizeX, sizeY) _) color steps initialWorld depict respond evolve = 
	playIO display color steps initialWorld 
	(\world -> do x <- depict world; return $ Translate (fromIntegral $ -sizeX `div` 2) (fromIntegral $ -sizeY `div` 2) x;)
	(\event -> \world -> respond (translateEvent (fromIntegral $ sizeX `div` 2) (fromIntegral $ sizeY `div` 2) event) world)
	evolve 
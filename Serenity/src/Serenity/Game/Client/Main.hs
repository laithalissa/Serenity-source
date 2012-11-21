module Serenity.Game.Client.Main (
	main
) 
where

import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.World
import Serenity.Game.Client.Controller

main :: IO ()
main = playIO
	(InWindow "Serenity Client" (1024, 768) (0, 0))
	white
	20
	initWorld
	drawWorld
	handleEvent
	handleStep

handleEvent :: Event -> World -> IO World
handleEvent event world = do
	newWorld <- return $ newWorldFromEvent event world

	-- ...
	-- send messages to server
	-- ...

	return newWorld

handleStep :: Float -> World -> IO World
handleStep delta world = do
	-- ...
	-- handle new messages from server
	-- ...

	newWorld <- return $ world

	return newWorld

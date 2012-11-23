module Serenity.Game.Client.Main (
	main
) 
where

import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.ClientState
import Serenity.Game.Client.Controller

main :: IO ()
main = playIO
	(InWindow "Virtual Balloon Commander" (1024, 768) (0, 0))
	white
	20
	initClientState
	drawClientState
	handleEvent
	handleStep

handleEvent :: Event -> ClientState -> IO ClientState
handleEvent event clientState = do
	newClientState <- return $ newClientStateFromEvent event clientState 

	-- ...
	-- send messages to server
	-- ...

	return newClientState 

handleStep :: Float -> ClientState -> IO ClientState
handleStep delta clientState = do
	-- ...
	-- handle new messages from server
	-- ...

	return clientState

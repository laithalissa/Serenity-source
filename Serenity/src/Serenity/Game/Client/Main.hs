module Serenity.Game.Client.Main where

import Serenity.External
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.Controller
import Serenity.Game.Client.KeyboardState
import Serenity.Model
import Serenity.Network.Connection
import Serenity.Network.Transport
import Serenity.Network.Utility

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game
import GHC.Float
-- | Run the client
client ::
	String -- ^ Host
	-> Int -- ^ Port 
	-> Int -- ^ Player Name
	-> IO () 
client serverHost serverPort ownerId = do

	print $ "Connecting... " ++ serverHost ++ ":" ++ show serverPort

	channels <- connectTo serverHost (fromIntegral serverPort)

	waitUntilConnected (channelConnection channels)
	print "Connected!"
	assets <- initAssets
	gameBuilder <- makeDemoGameBuilder

	playIO
		(InWindow "Project Serenity" windowSize (0, 0))
		black
		30
		(initClientState assets gameBuilder ownerId channels)
		(return . render)
		(\e c -> return $ handleEvent e c)
		(handleStep)
	where
		waitUntilConnected connTVar = do
			connection <- atomically $ readTVar connTVar
			if isConnected connection
				then return ()
				else threadDelay 10000 >> waitUntilConnected connTVar

-- | Handle a Gloss input event, e.g. keyboard action
-- The event is used to create a list of commands which are sent to the server.
-- For example, clicking in the game area will order a ship to move to that location.
handleEvent :: Event -> ClientState -> ClientState
handleEvent event = execState $ do
	id %= handleInput (translateEvent event)
	clientKeyboardState %= getNewKeyboardState event 
	where
		getNewKeyboardState (EventKey key keystate _ _) = handleKeyEvent key keystate
		getNewKeyboardState _ = id

-- | Update the game state on a time step
-- Updates are received from the server and then applied to the game state
handleStep :: Float -> ClientState -> IO ClientState
handleStep delta clientState = do

	let inbox = channelInbox $ clientState^.clientChannels
	let outbox = channelOutbox $ clientState^.clientChannels

	let messagesOut = map (\c -> CommandMessage c 0 0) (clientState^.clientCommands)
	-- Send commands to the server
	sendMessages outbox messagesOut

	-- Receive updates from the server
	messages <- readTChanUntilEmpty inbox
	let us = concatMap getUpdate messages

	-- Apply the updates to the game state
	gameState' <- return $ gameTime +~ (float2Double delta) $ updates us (clientState^.clientGame)

	if UpdateGameOver `elem` us
		then print $ "Game over! " ++ (show $ gameState'^.gameRanks)
		else return ()

	return $ (clientUIState.uiStateViewport .~ newViewPort $ clientState) {_clientGame = gameState', _clientCommands = []}

	where
		getUpdate (UpdateMessage update _) = [update]
		getUpdate _ = []

		ks = clientState^.clientKeyboardState
		left  = keyMostRecentDownFrom ks (Char 'a') [Char 'a', Char 'd']
		right = keyMostRecentDownFrom ks (Char 'd') [Char 'a', Char 'd']
		up    = keyMostRecentDownFrom ks (Char 'w') [Char 'w', Char 's']
		down  = keyMostRecentDownFrom ks (Char 's') [Char 'w', Char 's']
		inn   = keyMostRecentDownFrom ks (Char 'q') [Char 'q', Char 'e']
		out   = keyMostRecentDownFrom ks (Char 'e') [Char 'q', Char 'e']

		a = 3
		b = 0.1

		moveLeft   = if left  then modify (\((x,y), z) -> ((x-a,y), z) ) else return ()
		moveRight  = if right then modify (\((x,y), z) -> ((x+a,y), z) ) else return ()
		moveUp     = if up    then modify (\((x,y), z) -> ((x,y+a), z) ) else return ()
		moveDown   = if down  then modify (\((x,y), z) -> ((x,y-a), z) ) else return ()
		zoomIn     = if inn   then modify (\((x,y), z) -> ((x,y), z+b) ) else return ()
		zoomOut    = if out   then modify (\((x,y), z) -> ((x,y), z-b) ) else return ()
		allUpdates = do moveLeft; moveRight; moveUp; moveDown; zoomIn; zoomOut
		newViewPort = execState allUpdates $ clientState^.clientUIState.uiStateViewport

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
		(initClientState assets gameBuilder ownerId [(ownerId, "")] channels)
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

	let clientState' = if UpdateGameOver `elem` us
		then clientGameStatus .~ Complete $ clientState
		else clientState

	return $ (wasdControls clientState') {_clientGame = gameState', _clientCommands = []}

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

		left'  = keyMostRecentDownFrom ks (Char 'A') [Char 'A', Char 'D']
		right' = keyMostRecentDownFrom ks (Char 'D') [Char 'A', Char 'D']
		up'    = keyMostRecentDownFrom ks (Char 'W') [Char 'W', Char 'S']
		down'  = keyMostRecentDownFrom ks (Char 'S') [Char 'W', Char 'S']

		a = 10

		viewPortL = clientUIState.uiStateViewport

		wasdControls :: ClientState -> ClientState
		wasdControls = execState $ do
			sector <- use $ clientGame.gameBuilder.gbSector
			when right  $ viewPortL %= viewPortPanX sector ( a)
			when left   $ viewPortL %= viewPortPanX sector (-a)
			when up     $ viewPortL %= viewPortPanY sector ( a)
			when down   $ viewPortL %= viewPortPanY sector (-a)

			when right' $ viewPortL %= viewPortPanX sector ( a*4)
			when left'  $ viewPortL %= viewPortPanX sector (-a*4)
			when up'    $ viewPortL %= viewPortPanY sector ( a*4)
			when down'  $ viewPortL %= viewPortPanY sector (-a*4)

			when inn    $ viewPortL %= viewPortZoom sector (1.1)
			when out    $ viewPortL %= viewPortZoom sector (0.9)


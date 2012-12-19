module Serenity.Game.Client.Main (
	client
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.Assets (Assets)
import qualified Serenity.Game.Client.Assets as Assets
import Serenity.Game.Client.ClientState (ClientState(..), windowSize, viewPort)
import qualified Serenity.Game.Client.ClientState as ClientState
import Serenity.Game.Client.Controller

import Serenity.Game.Shared.GameStateUpdate (manyUpdateGameState)
import Serenity.Game.Shared.Model.Common (OwnerId)
import Serenity.Game.Shared.Model.GameMap (exampleGameMap)

import Serenity.Network.Connection
import Serenity.Network.Message (Message(..))
import Serenity.Network.Transport
import Serenity.Network.Utility

import Serenity.Game.Client.KeyboardState
import Control.Monad.State

-- | Run the client
client ::
	String    -- ^ Host
	-> Int    -- ^ Port 
	-> String -- ^ Player Name
	-> IO () 
client serverHost serverPort name = do

	print $ "Connecting... " ++ serverHost ++ ":" ++ show serverPort

	channels <- connectTo serverHost (fromIntegral serverPort)
	let inbox = channelInbox channels
	let outbox = channelOutbox channels

	waitUntilConnected (channelConnection channels)
	print "Connected!"

	assets <- Assets.initialize
	playIO
		(InWindow "Project Serenity" windowSize (0, 0))
		black
		30
		(initClientState assets name)
		(return . render)
		(handleEvent outbox)
		(handleStep inbox)

	where
		waitUntilConnected connTVar = do
			connection <- atomically $ readTVar connTVar
			if isConnected connection
				then return ()
				else threadDelay 10000 >> waitUntilConnected connTVar

-- | Create the initial client state
initClientState :: Assets -> OwnerId -> ClientState
initClientState assets name = ClientState.initialize assets exampleGameMap name

-- | Handle a Gloss input event, e.g. keyboard action
-- The event is used to create a list of commands which are sent to the server.
-- For example, clicking in the game area will order a ship to move to that location.
handleEvent :: TChan Message -> Event -> ClientState -> IO ClientState
handleEvent outbox event clientState = do
	-- Get a list of commands
	newClientState <- return $ handleInput (translateEvent event) clientState
	let messages = map (\c -> CommandMessage c 0 0) (commands newClientState)

	-- Send commands to the server
	sendMessages outbox messages

	return $ newClientState { ClientState.commands = [], keyboardState = getNewKeyboardState event (keyboardState newClientState) }

	where
		translateEvent (EventKey key state modifiers (x, y)) = EventKey key state modifiers (x + wx, y + wy)
		translateEvent (EventMotion (x, y)) = EventMotion (x + wx, y + wy)

		wx = fromIntegral $ (fst windowSize) `div` 2
		wy = fromIntegral $ (snd windowSize) `div` 2

		getNewKeyboardState (EventKey key keystate _ _) = handleKeyEvent key keystate
		getNewKeyboardState _ = id

-- | Update the game state on a time step
-- Updates are received from the server and then applied to the game state
handleStep :: TChan Message -> Float -> ClientState -> IO ClientState
handleStep inbox delta clientState = do
	-- Receive updates from the server
	messages <- readTChanUntilEmpty inbox
	let updates = concatMap getUpdate messages

	-- Apply the updates to the game state
	gameState' <- return $ manyUpdateGameState updates (gameState clientState)
	return $ clientState { gameState = gameState', uiState = (uiState clientState){viewPort = newViewPort} }

	where
		getUpdate (UpdateMessage update _) = [update]
		getUpdate _ = []

		ks = keyboardState clientState
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
		newViewPort = execState allUpdates (viewPort $ uiState clientState)

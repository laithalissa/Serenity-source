module Serenity.Game.Client.Main (
	client
)
where

import Control.Concurrent.STM
import Control.Monad (when)
import Graphics.Gloss.Interface.IO.Game
import Network.Socket (PortNumber (..))

import Serenity.Game.Client.Assets (Assets)
import qualified Serenity.Game.Client.Assets as Assets
import Serenity.Game.Client.ClientState (ClientState(..), windowSize)
import qualified Serenity.Game.Client.ClientState as ClientState
import Serenity.Game.Client.Controller

import Serenity.Game.Shared.GameStateUpdate (manyUpdateGameState)
import Serenity.Game.Shared.Model.Common (OwnerId)
import Serenity.Game.Shared.Model.GameMap (exampleGameMap)

import Serenity.Network.Message (Message(..), Command)
import Serenity.Network.Utility

-- | Run the client
client ::
	String    -- ^ Host
	-> Int    -- ^ Port 
	-> String -- ^ Player Name
	-> IO () 
client serverHost serverPort name = do

	print $ "Connecting... " ++ serverHost ++ ":" ++ show serverPort

	transport <- connectChannelsIO serverHost (fromIntegral serverPort)
	let inbox = channelInbox transport
	let outbox = channelOutbox transport

	print "Connected!"

	assets <- Assets.initialize
	playIO
		(InWindow "Project Serenity" windowSize (0, 0))
		white
		30
		(initClientState assets name)
		(return . render)
		(handleEvent outbox)
		(handleStep inbox)

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

	return $ newClientState { ClientState.commands = [] }

	where
		translateEvent (EventKey key state modifiers (x, y)) = EventKey key state modifiers (x + wx, y + wy)
		translateEvent (EventMotion (x, y)) = EventMotion (x + wx, y + wy)

		wx = fromIntegral $ (fst windowSize) `div` 2
		wy = fromIntegral $ (snd windowSize) `div` 2

-- | Update the game state on a time step
-- Updates are received from the server and then applied to the game state
handleStep :: TChan Message -> Float -> ClientState -> IO ClientState
handleStep inbox delta clientState = do
	-- Receive updates from the server
	messages <- readTChanUntilEmpty inbox
	let updates = concatMap getUpdate messages

	-- Apply the updates to the game state
	gameState' <- return $ manyUpdateGameState updates (gameState clientState)
	return $ clientState { gameState = gameState' }

	where
		getUpdate (UpdateMessage update _) = [update]
		getUpdate _ = []

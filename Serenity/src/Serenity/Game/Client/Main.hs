module Serenity.Game.Client.Main (
	main
)
where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (when)
import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.Assets (Assets)
import qualified Serenity.Game.Client.Assets as Assets
import Serenity.Game.Client.ClientState (ClientState(..), UIState(..))
import qualified Serenity.Game.Client.ClientState as ClientState
import Serenity.Game.Client.Common
import Serenity.Game.Client.Controller

import Serenity.Game.Shared.GameStateUpdate (manyUpdateGameState)
import Serenity.Game.Shared.Model.GameMap (exampleGameMap)

import Serenity.Network.Message (Message(..), Command, Update)
import Serenity.Network.Utility (TransportInterface(..), connectChannelsIO, readTChanUntilEmpty)

main :: IO ()
main = do
	assets <- Assets.initialize
	transport <- connectChannelsIO "localhost" 8080
	let inbox = channelInbox transport
	let outbox = channelOutbox transport

	playIO
		(InWindow "Virtual Balloon Commander" windowSize (0, 0))
		white
		20
		(initClientState assets)
		(return . render)
		(handleEvent outbox)
		(handleStep inbox)

-- | Create the initial client state
initClientState :: Assets -> ClientState
initClientState assets = ClientState.initialize assets exampleGameMap

handleEvent :: TChan Message -> Event -> ClientState -> IO ClientState
handleEvent outbox event clientState = do
	-- Get a list of commands
	newClientState <- return $ handleInput (translateEvent event) clientState
	let commands = ClientState.commands newClientState

	-- Send commands to the server
	sendCommands outbox commands

	when (not $ null commands) $ print commands

	return $ newClientState { ClientState.commands = [] }

	where
		translateEvent (EventKey key state modifiers (x, y)) = EventKey key state modifiers (x + wx, y + wy)
		translateEvent (EventMotion (x, y)) = EventMotion (x + wx, y + wy)

		wx = fromIntegral $ (fst windowSize) `div` 2
		wy = fromIntegral $ (snd windowSize) `div` 2

handleStep :: TChan Message -> Float -> ClientState -> IO ClientState
handleStep inbox delta clientState = do
	-- Receive updates from the server
	messages <- readTChanUntilEmpty inbox []
	let updates = map extractUpdate messages

	-- Apply the updates to the game state
	gameState' <- return $ manyUpdateGameState updates (gameState clientState)
	return $ clientState { gameState = gameState' }

	where
		extractUpdate (UpdateMessage update _) = update

sendCommands :: TChan Message -> [Command] -> IO ()
sendCommands _ [] = return ()
sendCommands outbox (c:cs) = do
	atomically $ writeTChan outbox (CommandMessage c 0 0)
	sendCommands outbox cs


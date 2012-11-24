module Serenity.Game.Client.Main (
	main
)
where

import Control.Monad (when)
import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.Assets (Assets)
import qualified Serenity.Game.Client.Assets as Assets
import Serenity.Game.Client.ClientState (ClientState(..), UIState(..))
import qualified Serenity.Game.Client.ClientState as ClientState
import Serenity.Game.Client.Common
import Serenity.Game.Client.Controller

import Serenity.Game.Shared.Model.GameMap (exampleGameMap)

main :: IO ()
main = do
	assets <- Assets.initialize
	playIO
		(InWindow "Virtual Balloon Commander" windowSize (0, 0))
		white
		20
		(initClientState assets)
		(return . render)
		handleEvent
		handleStep

-- | Create the initial client state
initClientState :: Assets -> ClientState
initClientState assets = ClientState.initialize assets exampleGameMap

handleEvent :: Event -> ClientState -> IO ClientState
handleEvent event clientState = do
	newClientState <- return $ handleInput (translateEvent event) clientState

	-- ...
	-- send commands to server
	-- ...

	when (not $ null (ClientState.commands newClientState)) $
		print $ ClientState.commands newClientState

	return $ newClientState { ClientState.commands = [] }

	where
		translateEvent (EventKey key state modifiers (x, y)) = EventKey key state modifiers (x + wx, y + wy)
		translateEvent (EventMotion (x, y)) = EventMotion (x + wx, y + wy)

		wx = fromIntegral $ (fst windowSize) `div` 2
		wy = fromIntegral $ (snd windowSize) `div` 2

handleStep :: Float -> ClientState -> IO ClientState
handleStep delta clientState = do
	-- ...
	-- handle new messages from server
	-- ...

	return clientState

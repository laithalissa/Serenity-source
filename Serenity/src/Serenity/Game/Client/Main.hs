module Serenity.Game.Client.Main (
	main
)
where

import Graphics.Gloss.Interface.IO.Game

import Serenity.Game.Client.ClientState (Game, render, step, handleInput)
import qualified Serenity.Game.Client.ClientState as ClientState
import Serenity.Game.Client.Controller
import Serenity.Game.Client.Assets (Assets(..), initialize)
import qualified Serenity.Game.Client.Assets as Assets
import Serenity.Game.Model.GameMap (exampleGameMap)

main :: IO ()
main = do
	assets <- assetsIO
	let windowSize = (1024, 768)
	playIO
		(InWindow "Virtual Balloon Commander" windowSize (0, 0))
		white
		20
		(createGame assets windowSize)
		(return . render)
		(\event w -> return $ handleInput event w)
		(\f w -> return $ step f w)

createGame :: Assets.Assets -> (Int, Int) -> Game
createGame assets windowSize = ClientState.initialize assets windowSize exampleGameMap

assetsIO = Assets.initialize :: IO Assets.Assets

--handleInputAndIO :: Event -> ClientState -> IO ClientState
--handleInputAndIO event clientState = do
--	newClientState <- return $ newClientStateFromEvent event clientState

--	-- ...
--	-- send commands to server
--	-- ...

--	return newClientState

--handleStep :: Float -> ClientState -> IO ClientState
--handleStep delta clientState = do
--	-- ...
--	-- handle new messages from server
--	-- ...

--	return clientState



module Serenity.Game.Server.Game
(	Game
,	initialize
,	render
,	handleInput
,	step
) where

import Graphics.Gloss.Data.Picture(Picture)
import Graphics.Gloss.Interface.Pure.Game(Event)

import Serenity.Game.Server.Assets(Assets)
import qualified Serenity.Game.Server.Graphics as Graphics
import qualified Serenity.Game.Server.InputFilter  as InputFilter
import qualified Serenity.Game.Server.World as World

import Serenity.Game.Model.ClientMessage(ClientMessage(..))
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.GameMap(GameMap)


initialize :: Assets -> Graphics.WindowSize -> GameMap -> Game
render :: Game -> Picture
handleInput :: Event -> Game -> Game
step :: TimeDuration -> Game -> Game

data Game = 
	Game   
        {	world :: World.World
	,	graphics :: Graphics.Graphics
	,	inputFilter :: InputFilter.InputFilter
	}                 	
	deriving(Show, Eq)
	        

initialize assets windowSize gameMap =
	Game
        {	world=firstWorld
	,	graphics=Graphics.initialize firstWorld assets windowSize
	,	inputFilter=InputFilter.initialize
	}
	where
		firstWorld = World.initialize gameMap


render game = Graphics.render (world game) (graphics game)

handleInput event game =
	case (InputFilter.handleInput event . inputFilter) game of
		(Just clientMessage, newInputFilter) -> case clientMessage of
			ClientMessageGraphics graphicsMessage -> 
				game{graphics=((Graphics.handleMessage graphicsMessage . graphics) game)}
			ClientMessageWorld worldMessage ->  
					game{world=((World.handleMessage worldMessage . world) game)}
		(Nothing, newInputFilter) -> game{inputFilter=newInputFilter}

step timeDelta game = game{world=(World.step timeDelta (world game))}


module Serenity.Game.Server.Game
(	initialize
,	render
,	handleInput
,	step
) where

import Graphics.Gloss.Data.Picture(Picture)
import Graphics.Gloss.Interface.Pure.Game(Event)

import Serenity.Game.Server.Assets(Assets)
import Serenity.Game.Server.Graphics(WindowSize)
import Serenity.Game.Model.GameMap(GameMap)

initialize :: Assets -> WindowSize -> GameMap -> Game
render :: Game -> Picture
handleInput :: Event -> Game -> Game

class (World world, Graphics graphics, => Game game world graphics inputFilter | game -> world graphics inputFilter where
	gameInitialize :: DefaultAssets -> 
                          WindowSize -> 
                          GameMap -> 
                          game
	gameRender :: game -> Picture
	gameHandleInput :: Event -> game -> game 
	gameStep :: TimeDuration -> game -> game

	gameWorld :: game -> world
	gameGraphics :: game -> graphics
	gameInputFilter :: game -> inputFilter



data DefaultGame = 
	DefaultGame   
        {	defaultGameWorld :: DefaultWorld
	,	defaultGameGraphics :: DefaultGraphics                      
	,	defaultGameInputFilter :: DefaultInputFilter               
	}                 	
	deriving(Show, Eq)
	        

instance Game DefaultGame DefaultWorld DefaultGraphics DefaultInputFilter where        
	gameInitialize assets windowSize gameMap =
        	DefaultGame
                {	defaultGameWorld=(worldInitialize gameMap) :: DefaultWorld
		,	defaultGameGraphics=(graphicsInitialize (worldInitialize gameMap) assets windowSize) :: DefaultGraphics
		,	defaultGameInputFilter=inputFilterInitialize :: DefaultInputFilter
		}

	gameRender game = graphicsRender game (defaultGameGraphics game)
        
        gameHandleInput event game = 
		case (inputFilterHandleInput event . defaultGameInputFilter) game of
			(Just clientMessage, newInputFilter) -> case clientMessage of
				ClientMessageGraphics graphicsMessage -> 
					game{defaultGameGraphics=((graphicsHandleMessage graphicsMessage . defaultGameGraphics) game)}
				ClientMessageWorld worldMessage ->  
					game{defaultGameWorld=((worldHandleMessage worldMessage . defaultGameWorld) game)}
			(Nothing, newInputFilter) -> game{defaultGameInputFilter=newInputFilter}
                        
	gameStep timeDelta game =	game{defaultGameWorld=(worldStep timeDelta (defaultGameWorld game))}
        
        gameWorld = defaultGameWorld
        gameGraphics = defaultGameGraphics
        gameInputFilter = defaultGameInputFilter
        
        
	


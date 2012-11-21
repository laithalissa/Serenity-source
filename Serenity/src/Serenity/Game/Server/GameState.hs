{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Serenity.Game.Server.GameState where

import Graphics.Gloss.Interface.Pure.Game(play)
import Graphics.Gloss.Data.Picture(Picture, loadBMP, text, color)
import Graphics.Gloss.Data.Color(red)
import Graphics.Gloss.Interface.Pure.Game(SpecialKey(..), Key(..), Event(..), KeyState(..), MouseButton(..))

import Serenity.Game.Model.ClientMessage(ClientMessage(..), GraphicsMessage, WorldMessage)
import Serenity.Game.Model.WorldDelta(WorldDelta)
import Serenity.Game.Model.GameMap(GameMap)
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.ShipClass(ShipClass(..))  
import Serenity.Game.Model.Common(TimeDuration)
import Serenity.Game.Model.Entity(Entity)

import qualified Data.Map as Map

	
class Assets assetManager where
	assetsInitialize :: IO assetManager
        assetsGetPicture :: String -> assetManager -> Picture

class (Show world) => World world where
	worldInitialize :: GameMap -> world
	worldStep :: TimeDuration -> world -> world
	worldHandleMessage :: WorldMessage -> world -> world

class (Show inputFilter) => InputFilter inputFilter where
 	inputFilterInitialize :: inputFilter 
 	inputFilterHandleInput :: Event -> inputFilter -> (Maybe ClientMessage, inputFilter)
        
        
  
type WindowSize = (Int, Int)
class Graphics graphics where
	graphicsInitialize :: DefaultAssets -> WindowSize -> graphics
        graphicsHandleMessage :: GraphicsMessage -> graphics -> graphics
        graphicsRender :: (World world) => world -> graphics -> Picture
    
  
class (World world, Graphics graphics, InputFilter inputFilter) => Game game world graphics inputFilter | game -> world graphics inputFilter where
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
        


---------- Default Implementation ----------

data DefaultAssets = 
	DefaultAssets
        {	defaultAssetsPictures :: Map.Map String Picture
	} 
	deriving(Eq, Show)
                
instance Assets DefaultAssets where                
	assetsInitialize = do
		planet1 <- loadBMP "planet1.bmp"        	              
                background <- loadBMP "background.bmp"
                ship1 <- loadBMP "ship1.bmp"
                ship2 <- loadBMP "ship2.bmp"
                assets <- return $ Map.fromList [ ("planet1", planet1)
                                                , ("background", background) 
                                                , ("ship1", ship1)  
                                                , ("ship2", ship2)  
                                                ]
                return DefaultAssets{defaultAssetsPictures=assets}
                
	assetsGetPicture name defaultAssetManager =                
		case (Map.lookup name (defaultAssetsPictures defaultAssetManager)) of
			Just asset -> asset                  
                        Nothing -> color red $ text ("Couldn't load asset " ++ name)
              

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
		,	defaultGameGraphics=(graphicsInitialize assets windowSize) :: DefaultGraphics
		,	defaultGameInputFilter=inputFilterInitialize :: DefaultInputFilter
		}                                    	

	gameRender game = graphicsRender (defaultGameWorld game) (defaultGameGraphics game)
        
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
        
        
data DefaultWorld =        
	DefaultWorld
        {	defaultWorldGameMap :: GameMap        	  
	,	defaultWorldEntities :: [Entity]                
	} deriving (Show, Eq)         	
        
instance World DefaultWorld where        
	worldInitialize gameMap = 
		DefaultWorld  
 		{	defaultWorldGameMap=gameMap               
		,	defaultWorldEntities=[]                                            
		}                                             		
        
	worldStep timeDelta world = world
        worldHandleMessage worldMessage world = world

data DefaultGraphics =
	DefaultGraphics  
	{	defaultGraphicsAssets :: DefaultAssets
	,	defaultGraphicsWindowSize :: WindowSize                
	} 
	deriving (Show, Eq)
                 
instance Graphics DefaultGraphics where                 

	graphicsInitialize assets windowSize =
		DefaultGraphics          
		{	defaultGraphicsAssets=assets                
		,	defaultGraphicsWindowSize=windowSize                                              
		}                                                  
                
	graphicsHandleMessage message graphics = graphics
        
        graphicsRender world graphics = color red $ text (show world)


data DefaultInputFilter =
	DefaultInputFilter  
	{	        
	}          
	deriving (Show, Eq)        
                 
instance InputFilter DefaultInputFilter where                 
	inputFilterInitialize = DefaultInputFilter  
        
	inputFilterHandleInput event inputFilter = (Nothing, inputFilter)        
        
        
        
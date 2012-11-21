{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Serenity.Game.Server.GameState where

import Graphics.Gloss.Interface.Pure.Game(play)
import Graphics.Gloss.Data.Picture(Picture(..), loadBMP, text, color, pictures, scale, translate, line, rotate)
import Graphics.Gloss.Data.Color(red)
import Graphics.Gloss.Interface.Pure.Game(SpecialKey(..), Key(..), Event(..), KeyState(..), MouseButton(..))

import Serenity.Game.Model.ClientMessage(ClientMessage(..), GraphicsMessage, WorldMessage)
import Serenity.Game.Model.WorldDelta(WorldDelta)
import Serenity.Game.Model.GameMap(GameMap(..), Planet(..), SpaceLane(..))
import Serenity.Game.Model.Common(TimeDuration, ViewPort)
import Serenity.Game.Model.ShipClass(ShipClass(..))  
import Serenity.Game.Model.Common(TimeDuration, toList4)
import Serenity.Game.Model.Entity(Entity(..))

import qualified Data.Map as Map

	
class Assets assetManager where
	assetsInitialize :: IO assetManager
        assetsGetPicture :: String -> assetManager -> Picture
	
	assetsGetPictureSize :: String -> Float -> Float -> assetManager -> Picture
	assetsGetPictureSize name nWidth nHeight assetManager = scaleBMPImage (nWidth, nHeight) (assetsGetPicture name assetManager)

class (Show world) => World world where
	worldInitialize :: GameMap -> world
	worldStep :: TimeDuration -> world -> world
	worldHandleMessage :: WorldMessage -> world -> world

	worldGameMap :: world -> GameMap

class (Show inputFilter) => InputFilter inputFilter where
 	inputFilterInitialize :: inputFilter 
 	inputFilterHandleInput :: Event -> inputFilter -> (Maybe ClientMessage, inputFilter)
        
        
  
type WindowSize = (Int, Int)
class Graphics graphics where
	graphicsInitialize :: DefaultAssets -> WindowSize -> graphics
        graphicsHandleMessage :: GraphicsMessage -> graphics -> graphics
        graphicsRender :: DefaultWorld -> graphics -> Picture
    
        graphicsViewPortSize :: graphics -> ViewPort

	graphicsViewPortX :: graphics -> Float
        graphicsViewPortX = (flip (!!)) 0 . toList4 . graphicsViewPortSize

	graphicsViewPortY :: graphics -> Float
        graphicsViewPortY = (flip (!!)) 1 . toList4 . graphicsViewPortSize

	graphicsViewPortWidth :: graphics -> Float
        graphicsViewPortWidth = (flip (!!)) 2 . toList4 . graphicsViewPortSize

	graphicsViewPortHeight :: graphics -> Float
        graphicsViewPortHeight = (flip (!!)) 3 . toList4 . graphicsViewPortSize
	
	graphicsWindowSize :: graphics -> WindowSize  

	graphicsWindowWidth :: graphics -> Float
        graphicsWindowWidth = fromIntegral . fst . graphicsWindowSize
	
	graphicsWindowHeight :: graphics -> Float
        graphicsWindowHeight = fromIntegral . snd . graphicsWindowSize

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

	worldGameMap = defaultWorldGameMap
	
	

data DefaultGraphics =
	DefaultGraphics  
	{	defaultGraphicsAssets :: DefaultAssets
	,	defaultGraphicsWindowSize :: WindowSize                
        ,	defaultGraphicsViewPort :: ViewPort
	} 
	deriving (Show, Eq)
                 
instance Graphics DefaultGraphics where                 

  	graphicsWindowSize = defaultGraphicsWindowSize
	graphicsViewPortSize = defaultGraphicsViewPort  

	graphicsInitialize assets windowSize =
		DefaultGraphics          
		{	defaultGraphicsAssets=assets                
		,	defaultGraphicsWindowSize=windowSize                                              
		,	defaultGraphicsViewPort=(0, 0, 100, 100)
                }                                                  
                
	graphicsHandleMessage message graphics = graphics
        
	graphicsRender world graphics = pictures 
		[	background
		,	(drawWorldToWindow . renderInWorld) world
		]
			where
				background = assetsGetPictureSize "background" ww wh (defaultGraphicsAssets graphics)
				drawWorldToWindow = translateWorld . scaleWorld
				scaleWorld = scale (ww/vpw) (wh/vph)
				translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))
				
                                ww = graphicsWindowWidth graphics
				wh = graphicsWindowHeight graphics
				vpx = graphicsViewPortX graphics
				vpy = graphicsViewPortY graphics 
				vpw = graphicsViewPortWidth graphics
				vph = graphicsViewPortHeight graphics
                          
				renderInWorld world = pictures $ 
					[	pictures $ map spaceLaneF (worldSpaceLanes world)
					,	pictures $ map planetF (worldPlanets world)
					,	pictures $ map entityF (worldEntities world) 
					]
					where
						worldSpaceLanes = gameMapSpaceLanes . worldGameMap
						worldPlanets = gameMapPlanets . worldGameMap
						worldEntities = defaultWorldEntities

				planetF planet = translate planetX planetY $ assetsGetPictureSize (planetType planet) 5 5 (defaultGraphicsAssets graphics) 
					where
						planetX =  (fst . planetLocation) planet
						planetY = (snd . planetLocation) planet

				spaceLaneF spaceLane@(SpaceLane p1N p2N) = line
					[ (pX p1N, pY p1N)
					, (pX p2N, pY p2N) 
                                        ]
                                                       
					where
						pX = fst . planetLocation . getPlanet
						pY = snd . planetLocation . getPlanet
						getPlanet name = foldl
							(\f s -> if (planetName f) == name
									then f
									else s)
							(head $ gameMapPlanets $ defaultWorldGameMap world)
							(gameMapPlanets $ defaultWorldGameMap world)
								
				entityF entity = case entity of
					Ship{} -> translate 
						(fst $ shipLocation entity) 
						(snd $ shipLocation entity)
						(assetsGetPicture "ship1" (defaultGraphicsAssets graphics))



data DefaultInputFilter =
	DefaultInputFilter  
	{	        
	}          
	deriving (Show, Eq)        
                 
instance InputFilter DefaultInputFilter where                 
	inputFilterInitialize = DefaultInputFilter  
        
	inputFilterHandleInput event inputFilter = (Nothing, inputFilter)        
        
        

scaleBMP :: Float -> Float -> Picture -> Picture
scaleBMP w h image@(Blank) = Blank



scaleBMPImage :: (Float, Float) -> Picture -> Picture
scaleBMPImage (nWidth, nHeight) image@(Blank) = image
scaleBMPImage (nWidth, nHeight) image@(Translate width height subImage) = 
	translate width height $ scaleBMPImage (nWidth, nHeight) subImage
scaleBMPImage (nWidth, nHeight) image@(Scale scaleX scaleY subImage) =
	scale scaleX scaleY $ scaleBMPImage (nWidth/scaleX, nHeight/scaleY) subImage
scaleBMPImage (nWidth, nHeight) image@(Rotate rotation subImage) =
	rotate rotation $ scaleBMPImage (nWidth, nHeight) subImage
scaleBMPImage (nWidth, nHeight) image@(Pictures subImages) =
	pictures $ map (scaleBMPImage (nWidth, nHeight)) subImages
scaleBMPImage (nWidth, nHeight) image@(Bitmap width height _ _) = 
	scale 
	(nWidth/(fromIntegral width)) 
	(nHeight/(fromIntegral height)) 
	image
        
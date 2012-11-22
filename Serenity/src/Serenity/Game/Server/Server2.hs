
module Serenity.Game.Server.Server2 where

import Control.Wire
import Prelude hiding ((.), id)
import Graphics.Gloss(Display(..))
import Graphics.Gloss.Interface.IO.Game(playIO)
import Graphics.Gloss.Data.Color(black, red)
import Graphics.Gloss.Data.Picture(Picture(Bitmap), text, color, loadBMP, scale, pictures, translate, line, Point, Path)
import Graphics.Gloss.Interface.Pure.Game(SpecialKey(..), Key(..), Event(..), KeyState(..), MouseButton(..))
import Data.Maybe(Maybe(..), fromJust)
import qualified Data.Map as Map

import Serenity.Game.Model.GameMap(GameMap(..), Planet(..), SpaceLane(..))
import Serenity.Game.Model.Common(Resources(..), TimeDuration, Location, Path, Polygon, Direction, Size, EntityId, ViewPort)
import Serenity.Game.Model.ShipClass(ShipClass(..), WeaponSlotType(..)) 
import Serenity.Game.Model.ClientMessage(ClientMessage(..))
import Serenity.Game.Model.ShipOrder(ShipOrder(..))
import Serenity.Game.Model.Entity(Entity(..))
import Serenity.Game.Server.KeyboardState(KeyboardState, initKeyboardState, handleKeyEvent, isKeyDown)
import Serenity.Game.Server.GameState(Assets(..), DefaultAssets(..))
import Serenity.Game.Server.GameState(Game(..), DefaultGame, Assets(..), DefaultAssets)

runWindowSize = (800, 600)
runWindowLocation = (100, 100)

main :: IO ()
main = do
	assets <- createAssets 
        play 
		gDisplay 
		gColor 
		gUPS 
		(gameInitialize runWindowSize gameMap) 
		gameRender 
		gameHandleInput 
		gameStep
  where
    createAssets :: IO DefaultAssets
    createAssets = assetsInitialize
    gDisplay = InWindow "Serenity"
                      runWindowSize
                      runWindowLocation
    gColor = black
    gUPS = 5
    gRender world = render world
    -- gInput event world = case event of
    --   EventKey key keyState modifiers (mouseX, mouseY) -> case (key, keyState) of
    --     (Char 'e', Down) -> updateFromCommand (zoom (-5)) world
    --     (Char 'q', Down) -> updateFromCommand (zoom 5) world        
    --     (Char 'w', Down) -> updateFromCommand (scroll 0 5) world
    --     (Char 's', Down) -> updateFromCommand (scroll 0 (-5)) world        
    --     (Char 'a', Down) -> updateFromCommand (scroll (-5) 0) world        
    --     (Char 'd', Down) -> updateFromCommand (scroll 5 0) world                
    --     (MouseButton LeftButton, Down) -> updateFromCommand (moveCommand mouseX mouseY) world
    --     _ -> return world
    --   _ -> return world  
    --   where  
    --     zoom delta = ClientScroll ((vx world) + (delta/2), 
    --                                (vy world) + (delta/2),
    --                                (vw world) - delta,
    --                                (vh world) - delta
    --                               )
    --     scroll dx dy = ClientScroll ((vx world) + dx,
    --                                     (vy world) + dy,
    --                                     (vw world),
    --                                     (vh world)
    --                                    ) 
	                                                                              
    --     moveCommand mx my = ClientMoveOrder 0 $ worldLocationFromWindow (mx, my) world

        
    --     vx SimpleWorld{worldViewPort=(x,y,w,h)} = x
    --     vy SimpleWorld{worldViewPort=(x,y,w,h)} = y
    --     vw SimpleWorld{worldViewPort=(x,y,w,h)} = w                                               
    --     vh SimpleWorld{worldViewPort=(x,y,w,h)} = h    
        
        
            
    -- gUpdate delta world = updateFromTimeDelta delta world
        
    gameMap = GameMap {
        gameMapName = "My First Map",
        gameMapSize = (100, 100),
        gameMapSpawnPoints=[(50, 50)],
        gameMapPlanets = [ createPlanet "Earth" (50, 50)                                             
                         , createPlanet "Mars" (10, 10)   
                         , createPlanet "Pluto" (90, 10)                                             ],
        gameMapSpaceLanes=[ SpaceLane "Earth" "Mars"
                          ]
        }
      where
        createPlanet name location = 
          Planet 
          {      planetName=name
          ,      planetType="planet1"                  
          ,      planetLocation=location                  
          ,      planetDirection=(0,1)                      
          ,      planetResources=Resources{fuel=10, antiMatter=10, metal=10}                       
          }        

        



-- loadAssets :: IO AssetManager
-- loadAssets = do
--   planet1 <- loadBMP "planet1.bmp"
--   background <- loadBMP "background.bmp"
--   ship1 <- loadBMP "ship1.bmp"
--   ship2 <- loadBMP "ship2.bmp"
--   return $ AssetManager (Map.fromList [       ("planet1", scaleBMPImage planetSize planet1)
--                         ,       ("background", background)
--                         ,       ("ship1", scaleBMPImage shipSize ship1) 
--                         ,       ("ship2", scaleBMPImage shipSize ship2)        
--                         ])
--                         (Map.fromList [])



-- planetSize :: (Float, Float)
-- planetSize = (5, 5)
      
-- shipSize ::(Float, Float)
-- shipSize = (3, 3)
      


---------- model ----------








      


    

---------- Simple World ----------


-- data SimpleWorld = SimpleWorld
--      {           worldGameMap :: GameMap
--      ,           worldEntities :: [Entity] 
--      ,           worldAssetManager :: AssetManager
--      ,           worldViewPort :: ViewPort
--      ,           worldWindowSize :: (Int, Int)                                     
--      ,           worldKeyboardState :: KeyboardState
--      } deriving(Show,Eq)

-- instance World SimpleWorld where
--   initialize assetManager windowSize gameMap = 
--     SimpleWorld { worldGameMap=gameMap
--                 , worldEntities=[ Ship {     shipId=0 
--                                   ,     shipLocation=(40,50)
--                                   ,     shipDirection=(0,1)                   
--                                   ,     shipSpeed=(0, 1) 
--                                   ,     shipOrder=StayStillOrder                       
--                                   }                
--                            ]               
--                 , worldAssetManager=assetManager                                
--                 , worldViewPort=(0.0, 0.0, (fst $ gameMapSize gameMap), (snd $  gameMapSize gameMap))                  
--                 , worldWindowSize=windowSize                
--                 , worldKeyboardState=initKeyboardState                  
--                 }
--   updateFromTimeDelta delta world = return world
--   updateFromCommand command world = case command of
--     ClientScroll viewport -> do
--       print $ "changing view port to " ++ (show viewport)
--       return world{worldViewPort=viewport}
--     _ -> return world
  
--   render world = do
--     finalWorldImage <- return $ (drawWorldToWindow . renderInWorld) world
--     background <- return $ scaleBMPImage (ww, wh) (getAssetW "background" world)
-- --    background <- return $ (getAssetW "background" world)
--     return $ pictures [background, finalWorldImage]


--     where
--       drawWorldToWindow = translateWorld . scaleWorld
--       scaleWorld = scale (ww/vpw) (wh/vph)
--       translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))
      
--       ww = windowWidth world
--       wh = windowHeight world
--       vpx = viewPortX world
--       vpy = viewPortY world      
--       vpw = viewPortWidth world
--       vph = viewPortHeight world
                          
--       renderInWorld world = pictures $ [ pictures $ map spaceLaneF (worldSpaceLanes world)
--                                        , pictures $ map planetF (worldPlanets world)
--                                        , pictures $ map entityF (worldEntities world) 
--                                        ]  
      
--       planetF planet = translate planetX planetY $ getAssetW (planetType planet) world
--         where
--           planetX = (fst . planetLocation) planet
--           planetY = (snd . planetLocation) planet
      
--       spaceLaneF spaceLane@(SpaceLane p1 p2) = line [ (pX p1, pY p1)
--                                                     , (pX p2, pY p2) 
--                                                     ]
                                                       
--         where
--           pX pName = (fst . planetLocation . getPlanet pName) world
--           pY pName = (snd . planetLocation . getPlanet pName) world
          
--       entityF entity = case entity of
--         Ship{} -> translate (fst $ shipLocation entity)
--                             (snd $ shipLocation entity) $ getAssetW "ship1" world




---------- SimpleWorld helper function ----------

-- toList2 (a1, a2) = [a1, a2]
-- toList3 (a1, a2, a3) = [a1, a2, a3]
-- toList4 (a1, a2, a3, a4) = [a1, a2, a3, a4]

-- viewPortLocation :: SimpleWorld -> Location
-- viewPortLocation world = (viewPortX, viewPortY)
--   where
--     viewPortX = toList4 (worldViewPort world) !! 0
--     viewPortY = toList4 (worldViewPort world) !! 1      
    
-- viewPortSize :: SimpleWorld -> Size    
-- viewPortSize world = (viewPortWidth, viewPortHeight)
--   where
--     viewPortWidth = toList4 (worldViewPort world) !! 2      
--     viewPortHeight = toList4 (worldViewPort world) !! 3      

-- viewPortX :: SimpleWorld -> Float
-- viewPortX = fst . viewPortLocation

-- viewPortY :: SimpleWorld ->Float

-- viewPortY = snd . viewPortLocation

-- viewPortWidth :: SimpleWorld -> Float
-- viewPortWidth = fst . viewPortSize

-- viewPortHeight :: SimpleWorld -> Float
-- viewPortHeight = snd . viewPortSize

-- windowSize :: SimpleWorld -> Size
-- windowSize SimpleWorld{worldWindowSize=(w,h)} = (fromIntegral w, fromIntegral h) 

-- windowWidth :: SimpleWorld -> Float
-- windowWidth = fst . windowSize

-- windowHeight :: SimpleWorld -> Float
-- windowHeight = snd . windowSize

-- worldLocationFromWindow :: (Float, Float) -> SimpleWorld -> (Float, Float)
-- worldLocationFromWindow (windowX, windowY) world = (worldX, worldY) 
--   where
--     worldX = (vw / ww)*(windowX + (ww/2)) + vx
--     worldY = (vh / wh)*(windowY + (wh/2)) + vy

--     ww = fst $ windowSize world
--     wh = snd $ windowSize world 
--     vx = fst $ viewPortLocation world
--     vy = snd $ viewPortLocation world
--     vw = fst $ viewPortSize world
--     vh = snd $ viewPortSize world

-- windowLocationFromWorld :: (Float, Float) -> SimpleWorld -> (Float, Float)
-- windowLocationFromWorld (worldX, worldY) world = (windowX, windowY)
--   where
--     windowX = ((ww * (worldX-vx)) / vw) - (ww/2)
--     windowY = ((wh * (worldY-vy)) / vh) - (wh/2)
    
--     ww = fst $ windowSize world
--     wh = snd $ windowSize world 
--     vx = fst $ viewPortLocation world
--     vy = snd $ viewPortLocation world
--     vw = fst $ viewPortSize world
--     vh = snd $ viewPortSize world

-- worldSize :: SimpleWorld -> Size
-- worldSize = gameMapSize . worldGameMap

-- getPlanet :: String -> SimpleWorld -> Planet
-- getPlanet name = fromJust . Map.lookup name . Map.fromList . map (\p->(planetName p, p)) . worldPlanets


-- worldPlanets :: SimpleWorld -> [Planet]
-- worldPlanets = gameMapPlanets . worldGameMap

-- worldSpaceLanes :: SimpleWorld -> [SpaceLane]
-- worldSpaceLanes = gameMapSpaceLanes . worldGameMap

-- scaleBMPImage :: (Float, Float) -> Picture -> Picture
-- scaleBMPImage (nWidth, nHeight) image@(Bitmap width height _ _) = scale (nWidth/(fromIntegral width)) 
--                                                                         (nHeight/(fromIntegral height)) 
--                                                                         image

-- getAssetW :: String -> SimpleWorld -> Picture
-- getAssetW name world = getAsset name (worldAssetManager world)

-- getAsset :: String -> AssetManager -> Picture
-- getAsset name assetManager = getPicture name assetManager
  

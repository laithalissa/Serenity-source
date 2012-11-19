
module Serenity.Game.Server.Demo where

import Control.Wire
import Prelude hiding ((.), id)
import Graphics.Gloss(Display(..))
import Graphics.Gloss.Interface.IO.Game(playIO)
import Graphics.Gloss.Data.Color(black, red)
import Graphics.Gloss.Data.Picture(Picture(Bitmap), text, color, loadBMP, scale, pictures, translate, line)
import Graphics.Gloss.Interface.Pure.Game(SpecialKey(..), Key(..), Event(..), KeyState(..), MouseButton(..))
import Data.Maybe(Maybe(..), fromJust)
import qualified Data.Map as Map

runWindowSize = (800, 600)

main :: IO ()
main = do
        assetManager <- loadAssets
        gWorld <- return (initialize assetManager runWindowSize gameMap :: SimpleWorld)
        playIO (gDisplay runWindowSize) gColor gUPS gWorld gRender gInput gUpdate
  where
    gDisplay windowSize = InWindow "Serenity"
                      windowSize 
                      (100,100)
    gColor = black
    gUPS = 5
    gRender world = render world
    gInput event world = case event of
      EventKey key keyState modifiers (mouseX, mouseY) -> case (key, keyState) of
        (Char 'e', Down) -> updateFromCommand (zoom (-5)) world
        (Char 'q', Down) -> updateFromCommand (zoom 5) world        
        (Char 'w', Down) -> updateFromCommand (scroll 0 5) world
        (Char 's', Down) -> updateFromCommand (scroll 0 (-5)) world        
        (Char 'a', Down) -> updateFromCommand (scroll (-5) 0) world        
        (Char 'd', Down) -> updateFromCommand (scroll 5 0) world                
        (MouseButton LeftButton, Down) -> do
          print $ "mouse at " ++ (show $ moveCommand mouseX mouseY)
          return world
        _ -> return world
      _ -> return world  
      where  
        zoom delta = ClientScroll ((vx world) + (delta/2), 
                                   (vy world) + (delta/2),
                                   (vw world) - delta,
                                   (vh world) - delta
                                  )
        scroll dx dy = ClientScroll ((vx world) + dx,
                                        (vy world) + dy,
                                        (vw world),
                                        (vh world)
                                       ) 
                                       
        moveCommand mx my = ClientMoveOrder 0 $ worldLocationFromWindow (mx, my) world

        
        vx SimpleWorld{worldViewPort=(x,y,w,h)} = x
        vy SimpleWorld{worldViewPort=(x,y,w,h)} = y
        vw SimpleWorld{worldViewPort=(x,y,w,h)} = w                                               
        vh SimpleWorld{worldViewPort=(x,y,w,h)} = h    
        
        
            
    gUpdate delta world = updateFromTimeDelta delta world
        
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
        createPlanet name location = Planet name "HUGE" location (0,1) (10, 20, 30)
        



loadAssets :: IO AssetManager
loadAssets = do
  planet1 <- loadBMP "planet1.bmp"
  background <- loadBMP "background.bmp"
  return $ Map.fromList [("planet1", planet1), ("background", background)]


---------- model ----------

type AssetManager = Map.Map String Picture
type EntityId = Int
type Location = (Float, Float)
type Direction = (Float, Float)
type Size = (Float, Float)
type TimeDuration = Float -- milliseconds
type Resources = (Int, Int, Int)
type ViewPort = (Float, Float, Float, Float)
data ClientMessage = ClientScroll ViewPort | 
                     ClientMoveOrder 
                     {               clientMoveOrderShipId :: EntityId 
                     ,               clientMoveOrderLocation :: Location
                     } |
                     ClientStillOrder { clientStillShipId :: EntityId }
                     deriving(Show, Eq)

data ShipOrder =
  StayStillOrder |
  MoveOrder { moveOrderLocation :: Location }
  deriving(Show, Eq)


class World a where
      initialize :: AssetManager -> (Int, Int) -> GameMap -> a
      updateFromTimeDelta :: TimeDuration -> a -> IO a
      updateFromCommand :: ClientMessage -> a -> IO a
      render :: a -> IO Picture
      
data GameMap = GameMap { 
  gameMapName :: String
, gameMapSize :: Size
, gameMapSpawnPoints :: [(Location)]
, gameMapPlanets :: [Planet]
, gameMapSpaceLanes :: [SpaceLane]
} deriving(Show, Eq)

data Planet = Planet { 
  planetName :: String
, planetType :: String -- specifies which size / texture to use
, planetLocation :: Location
, planetDirection :: Direction
, planetResources :: Resources
} deriving(Show, Eq)

data SpaceLane = SpaceLane 
     { spaceLanePlanet1 :: String
     , spaceLanePlanet2 :: String
     } deriving(Show, Eq)


---------- Entities ----------

data Entity = 
  Ship 
  {    shipId :: EntityId
  ,    shipLocation :: Location
  ,    shipDirection :: Direction
  ,    shipAcceleration :: Direction               
  ,    shipOrder :: ShipOrder
  } deriving (Show, Eq)
    

---------- Simple World ----------

planetSize :: (Float, Float)
planetSize = (5, 5)

data SimpleWorld = SimpleWorld
     {           worldGameMap :: GameMap
     ,           entities :: [Entity] 
     ,           worldAssetManager :: AssetManager
     ,           worldViewPort :: ViewPort
     ,           worldWindowSize :: (Int, Int) 
     } deriving(Show,Eq)

instance World SimpleWorld where
  initialize assetManager windowSize gameMap = 
    SimpleWorld { worldGameMap=gameMap
                , entities=[ Ship {     shipId=0 
                                  ,     shipLocation=(40,50)
                                  ,     shipDirection=(0,1)                   
                                  ,     shipAcceleration=(0, 1)                                                    
                                  ,     shipOrder=StayStillOrder                       
                                  }                
                           ]               
                , worldAssetManager=assetManager                                
                , worldViewPort=(0.0, 0.0, (fst $ gameMapSize gameMap), (snd $  gameMapSize gameMap))                  
                , worldWindowSize=windowSize                
                }
  updateFromTimeDelta delta world = return world
  updateFromCommand command world = case command of
    ClientScroll viewport -> do
      print $ "changing view port to " ++ (show viewport)
      return world{worldViewPort=viewport}
    _ -> return world
  
  render world = do
    finalWorldImage <- return $ (drawWorldToWindow . renderInWorld) world
    background <- return $ scaleBMPImage (ww, wh) (getAssetW "background" world)
    return $ pictures [background, finalWorldImage]


    where
      
      
      drawWorldToWindow = translateWorld . scaleWorld
      -- translateWorld = translate (-(windowWidth/2)) (-(windowHeight/2))
      -- scaleWorld = scale (windowWidth/worldWidth) (windowHeight/worldHeight)
      scaleWorld = scale (ww/vpw) (wh/vph)
      translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))
      
      ww = fst $ windowSize world
      wh = snd $ windowSize world
      vpx = fst $ viewPortLocation world
      vpy = snd $ viewPortLocation world      
      vpw = fst $ viewPortSize world
      vph = snd $ viewPortSize world
                          
      renderInWorld world = pictures $ [ pictures $ map spaceLaneF (worldSpaceLanes world)
                                       , pictures $ map planetF (worldPlanets world)
                                       ]  
      
      planetF planet = translate planetX planetY $ scaleBMPImage planetSize (getAssetW "planet1" world)
        where
          planetX = (fst . planetLocation) planet
          planetY = (snd . planetLocation) planet
      
      spaceLaneF spaceLane@(SpaceLane p1 p2) = line [ (pX p1, pY p1)
                                                    , (pX p2, pY p2) 
                                                    ]
        where
          pX pName = (fst . planetLocation . getPlanet pName) world
          pY pName = (snd . planetLocation . getPlanet pName) world




---------- SimpleWorld helper function ----------

toList2 (a1, a2) = [a1, a2]
toList3 (a1, a2, a3) = [a1, a2, a3]
toList4 (a1, a2, a3, a4) = [a1, a2, a3, a4]

viewPortLocation :: SimpleWorld -> Location
viewPortLocation world = (viewPortX, viewPortY)
  where
    viewPortX = toList4 (worldViewPort world) !! 0
    viewPortY = toList4 (worldViewPort world) !! 1      
    
viewPortSize :: SimpleWorld -> Size    
viewPortSize world = (viewPortWidth, viewPortHeight)
  where
    viewPortWidth = toList4 (worldViewPort world) !! 2      
    viewPortHeight = toList4 (worldViewPort world) !! 3      

windowSize :: SimpleWorld -> Size
windowSize SimpleWorld{worldWindowSize=(w,h)} = (fromIntegral w, fromIntegral h) 

worldLocationFromWindow :: (Float, Float) -> SimpleWorld -> (Float, Float)
worldLocationFromWindow (windowX, windowY) world = (worldX, worldY) 
  where
    worldX = (vw / ww)*(windowX + (ww/2)) + vx
    worldY = (vh / wh)*(windowY + (wh/2)) + vy

    ww = fst $ windowSize world
    wh = snd $ windowSize world 
    vx = fst $ viewPortLocation world
    vy = snd $ viewPortLocation world
    vw = fst $ viewPortSize world
    vh = snd $ viewPortSize world

windowLocationFromWorld :: (Float, Float) -> SimpleWorld -> (Float, Float)
windowLocationFromWorld (worldX, worldY) world = (windowX, windowY)
  where
    windowX = ((ww * (worldX-vx)) / vw) - (ww/2)
    windowY = ((wh * (worldY-vy)) / vh) - (wh/2)
    
    ww = fst $ windowSize world
    wh = snd $ windowSize world 
    vx = fst $ viewPortLocation world
    vy = snd $ viewPortLocation world
    vw = fst $ viewPortSize world
    vh = snd $ viewPortSize world

worldSize :: SimpleWorld -> Size
worldSize = gameMapSize . worldGameMap



getPlanet :: String -> SimpleWorld -> Planet
getPlanet name = fromJust . Map.lookup name . Map.fromList . map (\p->(planetName p, p)) . worldPlanets


worldPlanets :: SimpleWorld -> [Planet]
worldPlanets = gameMapPlanets . worldGameMap

worldSpaceLanes :: SimpleWorld -> [SpaceLane]
worldSpaceLanes = gameMapSpaceLanes . worldGameMap

scaleBMPImage :: (Float, Float) -> Picture -> Picture
scaleBMPImage (nWidth, nHeight) image@(Bitmap width height _ _) = scale (nWidth/(fromIntegral width)) 
                                                                        (nHeight/(fromIntegral height)) 
                                                                        image

getAssetW :: String -> SimpleWorld -> Picture
getAssetW name world = getAsset name (worldAssetManager world)

getAsset :: String -> AssetManager -> Picture
getAsset name assetManager = case (Map.lookup name assetManager) of
  Just asset -> asset
  Nothing -> color red $ text ("Couldn't load asset " ++ name)
  

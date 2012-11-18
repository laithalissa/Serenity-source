
module Serenity.Game.Server.Demo where

import Control.Wire
import Prelude hiding ((.), id)
import Graphics.Gloss(play, Display(..))
import Graphics.Gloss.Data.Color(black)
import Graphics.Gloss.Data.Picture(Picture, text,loadBMP)
import qualified Data.Map as Map

main :: IO ()
main = do
        assetManager <- loadAssets
        play gDisplay gColor gUPS gWorld (gRender assetManager) gInput gUpdate
  where
    gDisplay = InWindow "Serenity"
                      (500,500) 
                      (100,100)
    gColor = black
    gUPS = 5
    gWorld = "hello"
    gRender assetManager world = case (Map.lookup "planet1" assetManager) of
      Just planet1 -> planet1
      Nothing -> text "can't load planet"
    gInput input world = world
    gUpdate delta world = world
    
    gameMap = GameMap {
        gameMapName = "My First Map",
        gameMapSize = (100, 100),
        gameMapSpawnPoints=[(50, 50)],
        gameMapPlanets = [Planet {
                      planetName="Earth", 
                      planetType="HUGE", 
                      planetLocation=(50,50), 
                      planetDirection=(0,1), 
                      planetResources=(10, 20, 30)
                   }],
        gameMapSpaceLanes=[]
        }



loadAssets :: IO AssetManager
loadAssets = do
  planet1 <- loadBMP "planet1.bmp"
  return $ Map.fromList [("planet1", planet1)]


---------- model ----------

type AssetManager = Map.Map String Picture
type EntityId = Int
type Location = (Float, Float)
type Direction = (Float, Float)
type Size = (Int, Int)
type TimeDuration = Int -- milliseconds
type Resources = (Int, Int, Int)
data PlayerCommand = 
  StayStillCommand { stayStillShipId :: EntityId } |
  MoveToLocationCommand { moveToLocationCommandShipId :: EntityId, moveToLocationCommandTargetLocation :: Location }
  

class World a where
      initialize :: AssetManager -> GameMap -> a
      updateFromTimeDelta :: TimeDuration -> a -> a
      updateFromCommand :: PlayerCommand -> a -> a
      render :: a -> Picture
      
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


---------- Simple World ----------

data SimpleWorld = SimpleWorld
     { worldGameMap :: GameMap
     , worldAssetManager :: AssetManager                  
     } deriving(Show,Eq)

instance World SimpleWorld where
  initialize assetManager gameMap = 
    SimpleWorld { worldGameMap=gameMap
                , worldAssetManager=assetManager                                
                }
  updateFromTimeDelta delta world = world
  updateFromCommand command world = world
  render world = text "sup"

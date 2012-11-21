
module Serenity.Game.Server.Server where

import Control.Wire
import Prelude hiding ((.), id)
import Graphics.Gloss(Display(..), play)
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
import qualified Serenity.Game.Server.Assets as Assets

import Serenity.Game.Server.Game(Game, initialize, render, handleInput, step)

runWindowSize = (600, 400)

main :: IO ()
main = do
  assets <- assetsIO
  play (InWindow "Serenity" runWindowSize (100, 100)) black 5 (createGame assets) render handleInput step
  
  
createGame :: Assets.Assets -> Game
createGame assets = initialize assets runWindowSize gameMap
  
assetsIO = Assets.initialize :: IO Assets.Assets
  
gameMap = GameMap 
	{	gameMapName = "My First Map"
	,	gameMapSize = (100, 100)
	,	gameMapSpawnPoints=[(50, 50)]				   
	,	gameMapPlanets = 
			[	createPlanet "Earth" (50, 50)                                             
			,	createPlanet "Mars" (10, 10)   
                	,	createPlanet "Pluto" (90, 10)                                             
			]
	,	gameMapSpaceLanes=[SpaceLane "Earth" "Mars"]
        }
      where
        createPlanet name location = 
		Planet 
		{	planetName=name
		,	planetType="planet1"                  
		,	planetLocation=location
		,	planetDirection=(0,1)
		,	planetResources=Resources{fuel=10, antiMatter=10, metal=10}                       
		}

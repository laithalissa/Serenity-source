{-# LANGUAGE TemplateHaskell #-}

module Serenity.Game.Client.ClientState where

import Serenity.External
import Serenity.Game.Client.KeyboardState
import Serenity.Model hiding(Location, Direction)
import Serenity.Sheen 
import Serenity.Maths.Util 
import Serenity.Network.Transport

import Graphics.Gloss.Data.Extent
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- | The size of the Gloss window
windowSize :: (Int, Int)
windowSize = (1024, 750)

-- | The view port is the area of the game world that is being viewed
-- by the client. ((x, y), zoom)
type ViewPort = ((Double, Double), Double)

-- | A change in the view port's x and y coordinates
type ViewPortMove = (Double, Double)

-- | A change to the view port's zoom level
type ViewPortZoom = Double

type Location = (Double, Double)
type Size = (Double, Double)

-- | Convert a view port location into an in-game map location
mapLocationFromView
	:: Location -- ^ Location within the view port
	-> ViewPort -- ^ View port
	-> Size     -- ^ Size of the map
	-> Location

translatePoint (x, y) = (x + wx, y + wy)
translatePoint2 (x, y) = (x - wx, y - wy)

translateEvent (EventKey key state modifiers point) = EventKey key state modifiers (translatePoint point)
translateEvent (EventMotion point) = EventMotion (translatePoint point)
wx = fromIntegral $ (fst windowSize) `div` 2
wy = fromIntegral $ (snd windowSize) `div` 2

mapLocationFromView (x, y) ((vx, vy), vz) (w, h) = (mapX, mapY)
	where
		mapX = (-(vx*(1-s)) - (ww/2) + x)/s
		mapY = (-(vy*(1-s)) - (wh/2) + y)/s
		ww = fromIntegral $ fst windowSize
		wh = fromIntegral $ snd windowSize
		normScale = ((min ww wh) / (max w h))
		s = vz * normScale

mapExtentFromView :: Extent -> ViewPort -> Size -> Extent
mapExtentFromView extent viewPort mapSize = makeExtent (floor yMax') (floor yMin') (floor xMax') (floor xMin') where
	mapLocation x = mapLocationFromView x viewPort mapSize
	(xMin', yMin') = mapLocation $ pFloat2Double $ (fromIntegral xMin, fromIntegral yMin)
	(xMax', yMax') = mapLocation $ pFloat2Double $ (fromIntegral xMax, fromIntegral yMax)
	(yMax, yMin, xMax, xMin) = takeExtent extent

-- | Represents the state of the client including the current game state
-- and GUI's state
data ClientState = ClientState
	{	_clientGame :: Game                   -- ^ State of the game world, e.g. ship positions
	,	_clientUIState :: UIState ClientState -- ^ State of the GUI, e.g. view hierarchy
	,	_clientKeyboardState :: KeyboardState -- ^ What keys are down and in what order they went down
	,	_clientCommands :: [Command]          -- ^ List of commands to send to the server
	,	_clientAssets :: Assets
	,	_clientOwnerID :: OwnerID
	,	_clientChannels :: TransportInterface
	,	_clientGameStatus :: GameStatus
	}

data GameStatus = Playing | Complete deriving (Show, Eq)

data UIState a = UIState
	{	_uiStateViewport :: ViewPort
	,	_uiStateSelected :: Selection
	}

data Selection = SelectionOwnShips [Int] | SelectionEnemyShips [Int] | SelectionPlanet Int

selectionIsEmpty (SelectionOwnShips []) = True
selectionIsEmpty (SelectionEnemyShips []) = True
selectionIsEmpty _ = False

selectionToTriple :: Selection -> ([Int],[Int],Maybe Int)
selectionToTriple (SelectionOwnShips f)   = (f,[],Nothing)
selectionToTriple (SelectionEnemyShips e) = ([],e,Nothing)
selectionToTriple (SelectionPlanet i)     = ([],[],Just i)

makeLenses ''UIState
makeLenses ''ClientState

-- | Create the initial client state
initClientState :: Assets -> GameBuilder -> OwnerID -> [OwnerID] -> TransportInterface -> ClientState
initClientState assets gameBuilder ownerID players channels = ClientState
	{	_clientGame = game
	,	_clientUIState = initUIState game
	,	_clientKeyboardState = emptyKeyboardState
	,	_clientCommands = []
	,	_clientAssets = assets
	,	_clientOwnerID = ownerID
	,	_clientChannels = channels
	,	_clientGameStatus = Playing
	}
	where
		game = demoGame players gameBuilder

initUIState :: Game -> UIState ClientState
initUIState game = UIState
	{	_uiStateViewport = ((width/2, height/2), zoom)
	,	_uiStateSelected = SelectionOwnShips []
	}
	where
		(width, height) = game^.gameBuilder^.gbSector.sectorSize
		zoom = 1

lassoShips :: Extent -> ClientState -> ([Int],[Int],[Int],Bool)
lassoShips extent = evalState $ do
	viewPort    <- use (clientUIState.uiStateViewport)
	mapSize     <- use (clientGame.gameBuilder.gbSector.sectorSize)
	ships       <- use (clientGame.gameShips)
	oID         <- use clientOwnerID
	ext         <- return $ mapExtentFromView extent viewPort mapSize
	sUnderMouse <- return $ Map.filter (inBoxShip (expand ext)) ships
	(friendly, enemy) <- return $ partition (\(_,ship) -> ship^.ownerID == oID) (Map.toList sUnderMouse)
	planets     <- use (clientGame.gameBuilder.gbSector.sectorPlanets)
	pUnderMouse <- return $ Map.filter (inBoxPlanet (expand ext)) planets
	return $ (map fst friendly, map fst enemy, Map.keys pUnderMouse, wasDrag)
	where
		wasDrag = extentArea extent > 100
		expand extent  = if wasDrag then extent else expand' extent
		expand' extent = makeExtent (yMax+5) (yMin-5) (xMax+5) (xMin-5) where (yMax, yMin, xMax, xMin) = takeExtent extent

inBoxShip :: Extent -> Entity Ship -> Bool
inBoxShip extent ship = pointInExtent extent (pDouble2Float $ ship^.entityData.shipLocation)

inBoxPlanet :: Extent -> Planet -> Bool
inBoxPlanet extent planet = pointInExtent extent (pDouble2Float $ planet^.planetLocation)


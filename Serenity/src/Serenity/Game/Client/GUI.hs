module Serenity.Game.Client.GUI
(	render
,	handleMessage
)
where

import Serenity.Game.Client.Assets
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.ClientMessage (GUICommand(..))

import Serenity.Maths.Util
import Serenity.Model

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import GHC.Float

handleMessage :: GUICommand -> UIState ClientState -> UIState ClientState
--handleMessage (ClientScroll (dx, dy)) uiState@UIState{ viewport=((x, y), z) } = uiState { viewport = ((x+dx, y+dy), z) }
--handleMessage (ClientZoom dz) uiState@UIState{ viewport=(loc, z) } = uiState { viewport = (loc, z+dz) }
handleMessage _ uiState = uiState

render :: Game -> UIState ClientState -> Assets -> Picture
render game uiState assets = Pictures
	[	background
	,	(drawWorldToWindow . renderInWorld) game
	]
	where
		background = getPicture "background" assets
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld = scale (double2Float s) (double2Float s)
		translateWorld = translate (double2Float$ vpx*(1-s)) (double2Float$ vpy*(1-s))

		(ww, wh) = (fromIntegral w, fromIntegral h) where (w, h) = windowSize
		((vpx, vpy), vpz) = uiState^.viewport
		(gw, gh) =  game^.gameSector.sectorSize
		normScale = ((min ww wh) / (max gw gh))
		s = vpz * normScale

		renderInWorld game = pictures
			[	pictures $ map pictureSpaceLane $ game^.gameSector.sectorSpaceLanes
			,	pictures $ map picturePlanet $ Map.elems $ game^.gameSector.sectorPlanets
			,	pictures $ map pictureEntity $ Map.elems $ game^.gameShips
			]
			where
				worldSpaceLanes = game^.gameSector.sectorSpaceLanes
				worldPlanets = game^.gameSector.sectorPlanets

		picturePlanet planet = translate x y $ getPictureSized (planet^.planetEcotype.ecotypeAssetName) 5 5 assets where
			(x,y) = pDouble2Float $ planet^.planetLocation

		pictureSpaceLane (p1, p2) = color (dark green) $ line $ map (\p -> pDouble2Float $ p^.planetLocation) planets where
			planets = catMaybes $ map (\k -> Map.lookup k planetsMap) [p1, p2]
			planetsMap = game^.gameSector.sectorPlanets

		pictureEntity entity = translate x y $ rotate ((atan2 dx dy)/pi * 180) $ (getPictureSized "ship-commander" 10 10 assets) where
			(x,y) = pDouble2Float $ entity^.entityData.shipLocation
			(dx,dy) = pDouble2Float $ entity^.entityData.shipDirection
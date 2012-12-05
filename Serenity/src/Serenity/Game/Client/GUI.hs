module Serenity.Game.Client.GUI
(	render
,	handleMessage
)
where

import Graphics.Gloss.Data.Picture

import Serenity.Game.Client.Assets
import Serenity.Game.Client.ClientState (ClientState(..), UIState(..), windowSize)
import Serenity.Game.Client.ClientMessage (GUICommand(..))

import Serenity.Game.Shared.Model.Entity
import Serenity.Game.Shared.Model.GameMap
import Serenity.Game.Shared.Model.GameState (GameState, gameStateGameMap, gameStateEntities)

import qualified Data.Set as Set

handleMessage :: GUICommand -> UIState ClientState -> UIState ClientState
handleMessage (ClientScroll (dx, dy)) uiState@UIState{ viewPort=((x, y), z) } = uiState { viewPort = ((x+dx, y+dy), z) }
handleMessage (ClientZoom dz) uiState@UIState{ viewPort=(loc, z) } = uiState { viewPort = (loc, z+dz) }
handleMessage _ uiState = uiState

render :: GameState -> UIState ClientState -> Assets -> Picture
render gameState uiState assets = Pictures
	[	background
	,	(drawWorldToWindow . renderInWorld) gameState
	]
	where
		background = getPictureSized "background" ww wh assets
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld = scale s s
		translateWorld = translate (vpx*(1-s)) (vpy*(1-s))

		(ww, wh) = (fromIntegral w, fromIntegral h) where (w, h) = windowSize
		((vpx, vpy), vpz) = viewPort uiState
		(gw, gh) = gameMapSize $ gameStateGameMap gameState
		normScale = ((min ww wh) / (max gw gh))
		s = vpz * normScale

		renderInWorld gameState = pictures
			[	pictures $ map spaceLaneF (worldSpaceLanes gameState)
			,	pictures $ map planetF (worldPlanets gameState)
			,	pictures $ map entityF $ map entity (Set.toList $ worldEntities gameState)
			]
			where
				worldSpaceLanes = gameMapSpaceLanes . gameStateGameMap
				worldPlanets = gameMapPlanets . gameStateGameMap
				worldEntities = gameStateEntities

		planetF planet = translate planetX planetY $ getPictureSized (planetType planet) 5 5 assets where
			planetX =  (fst . planetLocation) planet
	 		planetY = (snd . planetLocation) planet

		spaceLaneF spaceLane@(SpaceLane p1N p2N) = line
				[	(pX p1N, pY p1N)
				,	(pX p2N, pY p2N)
				]
				where
					pX = fst . planetLocation . getPlanet
					pY = snd . planetLocation . getPlanet
					getPlanet name = foldl
						(\f s -> if (planetName f) == name
								then f
								else s)
						(head $ gameMapPlanets $ gameStateGameMap gameState)
						(gameMapPlanets $ gameStateGameMap gameState)
		entityF Ship{shipLocation=(x,y), shipDirection=(dx,dy)} = 
			translate x y $ (getPictureSized "ship-commander" 10 10 assets)

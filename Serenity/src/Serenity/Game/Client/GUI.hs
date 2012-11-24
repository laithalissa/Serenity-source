module Serenity.Game.Client.GUI
(	render
,	handleMessage
)
where

import Graphics.Gloss.Data.Picture

import Serenity.Game.Client.Assets
import Serenity.Game.Client.ClientMessage (GUICommand(..))
import Serenity.Game.Client.Common
import Serenity.Game.Client.UIState

import Serenity.Game.Shared.Model.Entity
import Serenity.Game.Shared.Model.GameMap
import Serenity.Game.Shared.Model.GameState (GameState, gameMap, entities)

handleMessage :: GUICommand -> UIState a -> UIState a
handleMessage (ClientScroll viewPortMove) uiState = uiState { viewPort = scrollViewPort viewPortMove (viewPort uiState) }
	where scrollViewPort (x, y) (vpx, vpy, vpw, vph) = (vpx + x, vpy + y, vpw, vph)

handleMessage (ClientZoom viewPortZoom) uiState = uiState { viewPort = zoomViewPort viewPortZoom (viewPort uiState) }
	where zoomViewPort (x, y, w, h) (vpx, vpy, vpw, vph) = (vpx + x, vpy + y, vpw + w, vph + h)

handleMessage message uiState = uiState

render :: GameState -> UIState a -> Assets -> Picture
render gameState uiState assets = Pictures
	[	background
	,	(drawWorldToWindow . renderInWorld) gameState
	]
	where
		background = getPictureSized "background" ww wh assets
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld = scale (ww/vpw) (wh/vph)
		translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))

		(ww, wh) = let (w, h) = windowSize in (fromIntegral w, fromIntegral h)
		(vpx, vpy, vpw, vph) = viewPort uiState

		renderInWorld gameState = pictures
			[	pictures $ map spaceLaneF (worldSpaceLanes gameState)
			,	pictures $ map planetF (worldPlanets gameState)
			,	pictures $ map entityF (worldEntities gameState)
			]
			where
			worldSpaceLanes = gameMapSpaceLanes . gameMap
			worldPlanets = gameMapPlanets . gameMap
			worldEntities = entities

		planetF planet = translate planetX planetY $ getPictureSized (planetType planet) 5 5 assets
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
						(head $ gameMapPlanets $ gameMap gameState)
						(gameMapPlanets $ gameMap gameState)
		entityF entity = case entity of
			Ship{} -> translate
				(fst $ shipLocation entity)
				(snd $ shipLocation entity)
				(getPicture "ship1" assets)

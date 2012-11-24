module Serenity.Game.Client.Graphics
(	Graphics
,	WindowSize
,	initialize
,	handleMessage
,	render
,	viewPort
,	windowSize
) where

import Graphics.Gloss.Data.Picture
	(	Picture
	,	line
	,	pictures
	,	translate
	,	scale
	)

import Serenity.Game.Shared.Model.ClientMessage(GraphicsMessage(..))
import Serenity.Game.Shared.Model.Common(ViewPort, toList4)
import Serenity.Game.Shared.Model.Entity(Entity(Ship, shipLocation))
import Serenity.Game.Shared.Model.GameMap
	(	GameMap
		(	gameMapSize
		,	gameMapPlanets
		,	gameMapSpaceLanes
		)
	,	Planet
		(	planetName
		,	planetType
		,	planetLocation
		)
	,	SpaceLane(SpaceLane)
	)
import Serenity.Game.Client.Assets(Assets, getPicture, getPictureSized)
import Serenity.Game.Shared.Model.GameState(GameState(gameMap, entities))

type WindowSize = (Int, Int)

data Graphics =
	Graphics
	{	assets :: Assets
	,	windowSize :: WindowSize
	,	viewPort :: ViewPort
	}
	deriving (Show, Eq)

initialize :: GameState -> Assets -> WindowSize -> Graphics
initialize world assets windowSize =
	Graphics
	{	assets=assets
	,	windowSize=windowSize
	,	viewPort=(0, 0, width, height)
	}
	where
		width = (fst . gameMapSize . gameMap) world
		height = (snd . gameMapSize . gameMap) world

handleMessage :: GraphicsMessage -> Graphics -> Graphics
handleMessage (ClientScroll viewport) graphics = graphics{viewPort=viewport}
handleMessage message graphics = graphics

render :: GameState -> Graphics -> Picture
render world graphics = pictures
	[	background
	,	(drawWorldToWindow . renderInWorld) world
	]
	where
		background = getPictureSized "background" ww wh (assets graphics)
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld = scale (ww/vpw) (wh/vph)
		translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))

		ww = windowWidth graphics
		wh = windowHeight graphics
		vpx = viewPortX graphics
		vpy = viewPortY graphics
		vpw = viewPortWidth graphics
		vph = viewPortHeight graphics

		renderInWorld world = pictures
			[	pictures $ map spaceLaneF (worldSpaceLanes world)
			,	pictures $ map planetF (worldPlanets world)
			,	pictures $ map entityF (worldEntities world)
			]
			where
			worldSpaceLanes = gameMapSpaceLanes . gameMap
			worldPlanets = gameMapPlanets . gameMap
			worldEntities = entities

		planetF planet = translate planetX planetY $ getPictureSized (planetType planet) 5 5 (assets graphics)
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
						(head $ gameMapPlanets $ gameMap world)
						(gameMapPlanets $ gameMap world)
		entityF entity = case entity of
			Ship{} -> translate
				(fst $ shipLocation entity)
				(snd $ shipLocation entity)
				(getPicture "ship1" (assets graphics))


viewPortX = (flip (!!)) 0 . toList4 . viewPort
viewPortY = (flip (!!)) 1 . toList4 . viewPort
viewPortWidth = (flip (!!)) 2 . toList4 . viewPort
viewPortHeight = (flip (!!)) 3 . toList4 . viewPort
windowWidth = fromIntegral . fst . windowSize
windowHeight = fromIntegral . snd . windowSize

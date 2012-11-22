
module Serenity.Game.Server.Graphics
(	Graphics
,	WindowSize
,	initialize
,	handleMessage
,	render
,	viewPort
,	windowSize	
) where

import Graphics.Gloss.Data.Picture(Picture)

import Serenity.Game.Model.ClientMessage(GraphicsMessage(..))
import Serenity.Game.Model.Common(ViewPort)
import Serenity.Game.Server.Assets(Assets, getPictureScaled)


type WindowSize = (Int, Int)

initialize :: World -> Assets -> WindowSize -> graphics
handleMessage :: GraphicsMessage -> graphics -> graphics
render :: Game -> graphics -> Picture

data Graphics =
	Graphics  
	{	assets :: Assets
	,	windowSize :: WindowSize
	,	viewPort :: ViewPort
	} 
	deriving (Show, Eq)

initialize world assets windowSize =
	Graphics          
	{	assets=assets
	,	windowSize=windowSize
	,	viewPort=(0, 0, width, height)
	}
	where
		width = (fst . gameMapSize . worldGameMap) world
		height = (snd . gameMapSize . worldGameMap) world

handleMessage (ClientScroll viewport) graphics = 
	graphics{graphicsViewPort=viewport}
handleMessage message graphics = graphics
        
render game graphics = pictures 
	[	background
	,	(drawWorldToWindow . renderInWorld) world
	]
	where
		world = defaultGameWorld game
		background = getPictureSized "background" ww wh (assets graphics)
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld = scale (ww/vpw) (wh/vph)
		translateWorld = translate (-((ww/vpw)*vpx + (ww/2))) (-((wh/vph)*vpy + (wh/2)))
				
		ww = graphicsWindowWidth graphics
		wh = graphicsWindowHeight graphics
		vpx = graphicsViewPortX graphics
		vpy = graphicsViewPortY graphics 
		vpw = graphicsViewPortWidth graphics
		vph = graphicsViewPortHeight graphics
                          
		renderInWorld world = pictures
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



viewPortX :: graphics -> Float
viewPortX = (flip (!!)) 0 . toList4 . graphicsViewPortSize

viewPortY :: graphics -> Float
viewPortY = (flip (!!)) 1 . toList4 . graphicsViewPortSize

viewPortWidth :: graphics -> Float
viewPortWidth = (flip (!!)) 2 . toList4 . graphicsViewPortSize

viewPortHeight :: graphics -> Float
viewPortHeight = (flip (!!)) 3 . toList4 . graphicsViewPortSize

windowWidth :: graphics -> Float
windowWidth = fromIntegral . fst . graphicsWindowSize
	
windowHeight :: graphics -> Float
windowHeight = fromIntegral . snd . graphicsWindowSize

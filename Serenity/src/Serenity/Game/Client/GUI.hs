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

		picturePlanet planet = translate x y $ getPictureSized (planet^.planetEcotype.ecotypeAssetName) 5 5 assets where
			(x,y) = pDouble2Float $ planet^.planetLocation

		pictureSpaceLane (p1, p2) = color (dark green) $ line $ map (\p -> pDouble2Float $ p^.planetLocation) planets where
			planets = catMaybes $ map (\k -> Map.lookup k planetsMap) [p1, p2]
			planetsMap = game^.gameSector.sectorPlanets

		pictureEntity entity = translate x y $ Pictures [rotate ((atan2 dx dy)/pi * 180) $ (getPictureSized "ship-commander" dim dim assets), 
														(translate (-boundingX / 2) 5 $ Pictures [boundingBox, 
														healthMeter]), 
														(translate (-boundingX / 2) 5.6 $ Pictures [boundingBox, 
														shieldMeter])] where
			(x,y) = pDouble2Float $ entity^.entityData.shipLocation
			(dx,dy) = pDouble2Float $ entity^.entityData.shipDirection
			dim = 10
			boundingBox = color (makeColor8 200 200 200 40) $ Polygon $ [(0,0), (boundingX, 0), (boundingX, boundingY), (0, boundingY)]
			healthMeter = color (healthColor healthPercentage) $ Polygon $ [(0,0), 
																(healthBarWidth, 0), 
																(healthBarWidth, boundingY), 
																(0, boundingY)]
			shieldMeter = color shieldBlue $ Polygon $ [(0,0), 
											(shieldBarWidth, 0), 
											(shieldBarWidth, boundingY), 
											(0, boundingY)]
			--healthBarWidth = 20
			healthBarWidth = boundingX - (lostHealthPerc * boundingX)
			boundingY = 0.5
			boundingX = 5
			--lostHealth = boundingX * healthValue
			healthValue = entity^.entityData.shipDamage.damageHull
			shipTotalHealth = entity^.entityData.shipType.classMaxDamage.damageHull
			lostHealth = shipTotalHealth - healthValue
			lostHealthPerc = fromIntegral lostHealth / fromIntegral shipTotalHealth
			healthPercentage = fromIntegral healthValue / fromIntegral shipTotalHealth
			--Shield shiz
			shieldBarWidth = boundingX - (lostShieldPerc * boundingX)
			shieldValue = entity^.entityData.shipDamage.damageShield
			shipTotalShield = entity^.entityData.shipType.classMaxDamage.damageShield
			lostShield = shipTotalShield - shieldValue
			lostShieldPerc = fromIntegral lostShield / fromIntegral shipTotalShield
			shieldPercentage = fromIntegral shieldValue / fromIntegral shipTotalShield
			-- Colour for the shields
			shieldBlue = makeColor8 0 0 99 60

healthColor :: Float -> Color 
healthColor health = case health of -- if health > 0.33 then (makeColor8 255 0 0 60)
	health | health < 0.2 -> (makeColor8 255 0 0 60)
	health | health < 0.5 -> (makeColor8 255 255 0 60)
	health | health <= 1 -> (makeColor8 41 181 16 60)
	health -> (makeColor8 0 0 255 100)


module Serenity.Game.Client.GUI
(	render
,	handleMessage
)
where

import AssetsManager
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.ClientMessage (GUICommand(..))

import Serenity.Maths.Util
import Serenity.Model

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
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

		pictureEntity :: Entity Ship -> Picture
		pictureEntity entity = pictures $ shipAndHealth ++ beams where
			beams = concatMap pictureBeam (entity^.entityData^.shipBeamTargets)
			pictureBeam target = case Map.lookup target (game^.gameShips) of
				Just entity -> [color red $ line [(x, y + 2), (pDouble2Float $ entity^.entityData.shipLocation)]]
				Nothing -> []

			shipAndHealth = map (translate x y) [rotate ((atan2 dx dy)/pi * 180) $ (getPictureSized "commander-green" dim dim assets), 
														(translate (-boundingBoxWidth / 2) 5 $ Pictures [boundingBox, 
														healthMeter]), 
														(translate (-boundingBoxWidth / 2) 5.6 $ Pictures [boundingBox, 
														shieldMeter])] where
			(x,y) = pDouble2Float $ entity^.entityData.shipLocation
			(dx,dy) = pDouble2Float $ entity^.entityData.shipDirection
			dim = 10
			-- Background box for health and shield meters
			boundingBox = color (makeColor8 200 200 200 40) $ Polygon $ [(0,0), (boundingBoxWidth, 0), (boundingBoxWidth, boxHeight), (0, boxHeight)]
			healthMeter = color (healthColorCont healthAsPercentage) $ Polygon $ [(0,0), 
																(healthBarWidth, 0), 
																(healthBarWidth, boxHeight), 
																(0, boxHeight)]
			shieldMeter = color shieldBlue $ Polygon $ [(0,0), 
											(shieldBarWidth, 0), 
											(shieldBarWidth, boxHeight), 
											(0, boxHeight)]
			healthBarWidth = boundingBoxWidth - (lostHealthAsPercentage * boundingBoxWidth)
			boxHeight = 0.5
			boundingBoxWidth = 5
			-- Ship health values
			totalHealth = (fromJust $ Map.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) (game^.gameShipClasses))^.shipClassDamageStrength^.damageHull
			----totalHealth = entity^.entityData.shipType.shipTypeMaxDamage.damageHull
			lostHealth = entity^.entityData.shipDamage.damageHull
			currentHealth = totalHealth - lostHealth
			healthAsPercentage = fromIntegral currentHealth / fromIntegral totalHealth
			lostHealthAsPercentage = fromIntegral lostHealth / fromIntegral totalHealth
			-- Shop shield values
			shieldBarWidth = boundingBoxWidth - (lostShieldPercentage * boundingBoxWidth)
			lostShield = entity^.entityData.shipDamage.damageShield
			shipTotalShield = (fromJust $ Map.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) (game^.gameShipClasses))^.shipClassDamageStrength^.damageShield
			----shipTotalShield = entity^.entityData.shipType.shipTypeMaxDamage.damageShield
			currentShield = shipTotalShield - lostShield
			lostShieldPercentage = fromIntegral lostShield / fromIntegral shipTotalShield
			shieldPercentage = fromIntegral currentShield / fromIntegral shipTotalShield
			-- Colour for the shields
			shieldBlue = makeColor8 0 0 99 100

healthColorCont :: Float -> Color 
healthColorCont health 
	| health <= rBoundary = (makeColor8 255 0 0 alpha)
	| health <= yBoundary = (makeColor8 255 (greenRatio health) 0 alpha)
	| otherwise = (makeColor8 (redRatio health) 255 0 alpha)
	where
		greenRatio h = floor ((h - rBoundary)/(yBoundary - rBoundary) * 255)
		redRatio h = 255 - floor ((h - yBoundary)/(gBoundary - yBoundary) * 255)
		alpha = 100
		rBoundary = 0.2
		yBoundary = 0.5
		gBoundary = 1

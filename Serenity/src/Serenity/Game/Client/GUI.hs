module Serenity.Game.Client.GUI
(	render
,	handleMessage
)
where

import Serenity.External
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.ClientMessage (GUICommand(..))
import Serenity.Maths.Util
import Serenity.Model

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import GHC.Float

import Control.Monad.State

isSelected :: Simple Lens (Ship, Game) Bool
isSelected = lens (\_ -> True) (\a _ -> a)

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
		(gw, gh) =  game^.gameBuilder^.gbSector.sectorSize
		normScale = ((min ww wh) / (max gw gh))
		s = vpz * normScale

		renderInWorld game = pictures
			[	pictures $ map pictureSpaceLane $ game^.gameBuilder^.gbSector.sectorSpaceLanes
			,	pictures $ map picturePlanet $ Map.elems $ game^.gameBuilder^.gbSector.sectorPlanets
			,	pictures $ map (pictureEntity (game^.gameTime)) $ Map.elems $ game^.gameShips
			]

		picturePlanet planet = translate x y $ getPictureSized (planet^.planetEcotype.ecotypeAssetName) 5 5 assets where
			(x,y) = pDouble2Float $ planet^.planetLocation

		pictureSpaceLane (p1, p2) = color (dark green) $ line $ map (\p -> pDouble2Float $ p^.planetLocation) planets where
			planets = catMaybes $ map (\k -> Map.lookup k planetsMap) [p1, p2]
			planetsMap = game^.gameBuilder^.gbSector.sectorPlanets

		pictureEntity :: Double -> Entity Ship -> Picture
		pictureEntity time entity = pictures $ (shipAndHealth time) ++ beams where
			beams = concatMap pictureBeam (entity^.entityData.shipBeamTargets)
			pictureBeam target = case Map.lookup target (game^.gameShips) of
				Just entity -> [color red $ line [(x, y + 2), (pDouble2Float $ entity^.entityData.shipLocation)]]
				Nothing -> []

			selection = if shipIsSelected then [drawSelectionArc 5 (double2Float time)] else []
			shipAndHealth time = map (translate x y) $
				selection ++ [rotate ((atan2 dx dy)/pi * 180) $ (getPictureSized "commander-green" dim dim assets), 
				(translate (-boundingBoxWidth / 2) 5 $ Pictures [boundingBox, 
				healthMeter]), 
				(translate (-boundingBoxWidth / 2) 5.6 $ Pictures [boundingBox, 
				shieldMeter])] where
			(x,y) = pDouble2Float $ entity^.entityData.shipLocation
			(dx,dy) = pDouble2Float $ entity^.entityData.shipDirection
			dim = 10
			shipIsSelected = (entity^.entityData, game)^.isSelected
			-- Background box for health and shield meters
			boundingBox = color (makeColor8 200 200 200 40) $ Polygon $ [(0,0), (boundingBoxWidth, 0), (boundingBoxWidth, boxHeight), (0, boxHeight)]
			healthMeter = color (healthColor healthAsPercentage) $ Polygon $ 
				[	(0,0)
				,	(healthBarWidth, 0)
				,	(healthBarWidth, boxHeight)
				,	(0, boxHeight)
				]
			shieldMeter = color shieldBlue $ Polygon $ 
				[	(0,0)
				,	(shieldBarWidth, 0)
				,	(shieldBarWidth, boxHeight)
				,   (0, boxHeight)
				]

			healthBarWidth = boundingBoxWidth - (lostHealthAsPercentage * boundingBoxWidth)
			boxHeight = 0.5
			boundingBoxWidth = 5
			-- Ship health values
			totalHealth = (fromJust $ Map.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) (game^.gameBuilder^.gbShipClasses))^.shipClassMaxDamage^.damageHull
			----totalHealth = entity^.entityData.shipType.shipTypeMaxDamage.damageHull
			lostHealth = entity^.entityData.shipDamage.damageHull
			currentHealth = totalHealth - lostHealth
			healthAsPercentage = fromIntegral currentHealth / fromIntegral totalHealth
			lostHealthAsPercentage = fromIntegral lostHealth / fromIntegral totalHealth
			-- Shop shield values
			shieldBarWidth = boundingBoxWidth - (lostShieldPercentage * boundingBoxWidth)
			lostShield = entity^.entityData.shipDamage.damageShield
			shipTotalShield = (fromJust $ Map.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) (game^.gameBuilder^.gbShipClasses))^.shipClassMaxDamage^.damageShield
			----shipTotalShield = entity^.entityData.shipType.shipTypeMaxDamage.damageShield
			currentShield = shipTotalShield - lostShield
			lostShieldPercentage = fromIntegral lostShield / fromIntegral shipTotalShield
			shieldPercentage = fromIntegral currentShield / fromIntegral shipTotalShield
			-- Colour for the shields
			shieldBlue = makeColor8 0 0 99 100

drawSelectionArc :: Float -> Float -> Picture
drawSelectionArc radius time = color (selectionColour time) $ rotate (time * 10) $ circle where
		arcLength = 10
		circle = Pictures $ map (\x -> (ThickArc x (x + arcLength) radius) arcThickness) [0, arcLength*2..(360 - arcLength*2)]
		selectionColour time = (makeColor8 red (pulsingColour greenBase time) blue alpha)
		pulsingColour base time = (base + (round (oscillationLimit * (sin $ 2 * time))))
		red = 100
		blue = 100
		-- Minimum amount of green so the pulsing doesn't overflow max (255)
		greenBase = (255 - (round oscillationLimit))
		alpha = 75
		oscillationLimit = 35
		arcThickness = 0.8

healthColor :: Float -> Color 
healthColor health 
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

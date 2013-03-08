{-# LANGUAGE RankNTypes #-}

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
import Serenity.Game.Client.Color

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import GHC.Float

import Control.Monad.State

isSelected :: UIState ClientState -> Getter (Entity Ship) Bool
isSelected uiState = to (\entity -> elem (entity^.entityID) (uiState^.uiStateSelected))

handleMessage :: GUICommand -> UIState ClientState -> UIState ClientState
--handleMessage (ClientScroll (dx, dy)) uiState@UIState{ viewport=((x, y), z) } = uiState { viewport = ((x+dx, y+dy), z) }
--handleMessage (ClientZoom dz) uiState@UIState{ viewport=(loc, z) } = uiState { viewport = (loc, z+dz) }
handleMessage _ uiState = uiState

render :: Game -> UIState ClientState -> Assets -> Picture
render game uiState assets = Pictures
	[	stars
	,   blueNebula
	,   greenNebula
	,	(drawWorldToWindow . renderInWorld) game
	]
	where
		greenNebula       = getPicture "greenNebulaLayer" assets
		blueNebula        = getPicture "blueNebulaLayer" assets
		stars             = getPicture "starBackdropLayer" assets 
		drawWorldToWindow = translateWorld . scaleWorld
		scaleWorld        = scale (double2Float s) (double2Float s)
		translateWorld    = translate (double2Float $ vpx*(1-s)) (double2Float$ vpy*(1-s))

		(ww, wh)           = (fromIntegral w, fromIntegral h) where (w, h) = windowSize
		((vpx, vpy), vpz)  = uiState^.uiStateViewport
		(gw, gh)           =  game^.gameBuilder^.gbSector.sectorSize
		normScale          = ((min ww wh) / (max gw gh))
		s                  = vpz * normScale

		renderInWorld game = pictures
			[	pictures $ map pictureSpaceLane $ game^.gameBuilder^.gbSector.sectorSpaceLanes
			,	pictures $ map picturePlanet $ Map.elems $ game^.gameBuilder^.gbSector.sectorPlanets
			,	pictures $ map (pictureEntity (game^.gameTime)) $ Map.elems $ game^.gameShips
			]

		picturePlanet planet = 
			translate x y 
			$ getPictureSized (planet^.planetEcotype.ecotypeAssetName) 15 15 assets where
				(x,y) = pDouble2Float $ planet^.planetLocation

		pictureSpaceLane (p1, p2) = 
			color (dark green) 
			$ line $ map (\p -> pDouble2Float $ p^.planetLocation) planets where
				planets    = catMaybes $ map (\k -> Map.lookup k planetsMap) [p1, p2]
				planetsMap = game^.gameBuilder^.gbSector.sectorPlanets

		pictureEntity :: Double -> Entity Ship -> Picture
		pictureEntity time entity = pictures $ (shipAndHealth time) ++ beams where
			beams = concatMap pictureBeam (concat . Map.elems $ entity^.entityData.shipTargets)
			pictureBeam target = case Map.lookup target (game^.gameShips) of
				Just entity -> [color red $ line [(x, y + 2),
					(pDouble2Float $ entity^.entityData.shipLocation)]]
				Nothing -> []

			shipIsSelected = entity^.(isSelected uiState)
			selection      = 
				if shipIsSelected 
				then [drawSelectionArc 5 (double2Float time)] 
				else []
			
			shipAndHealth time = map (translate x y) $
				selection ++ 
				[	rotate ((atan2 dx dy)/pi * 180) $ Pictures [shipBridge, (getPictureSized "transparent" dim dim assets)]
				,	(translate (-boundingBoxWidth / 2) 5 $ Pictures [boundingBox , healthMeter])
				,	(translate (-boundingBoxWidth / 2) 5.6 $ Pictures [boundingBox, shieldMeter])
				] where

			shipBridge = 
				translate (-0.052 * dim) (-0.47 * dim) 
				$ scale (0.105 * dim) (0.96 * dim) 
				$ color (ownerIDColor (entity^.ownerID)) 
				$ polygon [(0,0), (0,0.95), (0.5, 1), (1, 0.95), (1, 0)]
			
			(x,y)   = pDouble2Float $ entity^.entityData.shipLocation
			(dx,dy) = pDouble2Float $ entity^.entityData.shipDirection
			dim     = 10
			
			-- Background box for health and shield meters
			boundingBox = 
				color (makeColor8 200 200 200 40) 
				$ Polygon $ 
				[	(0,0)
				,	(boundingBoxWidth, 0)
				,	(boundingBoxWidth, boxHeight)
				,	(0, boxHeight)]
			
			healthMeter = color (healthColor healthAsPercentage) 
				$ Polygon $ 
				[	(0,0)
				,	(healthBarWidth, 0)
				,	(healthBarWidth, boxHeight)
				,	(0, boxHeight)
				]
			shieldMeter = color shieldBlue 
				$ Polygon $ 
				[	(0,0)
				,	(shieldBarWidth, 0)
				,	(shieldBarWidth, boxHeight)
				,   (0, boxHeight)
				]

			boundingBoxWidth = 5
			boxHeight        = 0.5
			healthBarWidth   = boundingBoxWidth - (lostHealthPercentage * boundingBoxWidth)

			-- Ship health values
			currentHealth        = shipHealth' entity game
			healthAsPercentage   = fromIntegral (shipHealth' entity game) / fromIntegral (shipMaxHealth' entity game)
			lostHealthPercentage = fromIntegral (shipCurrentDamage' entity) / fromIntegral (shipMaxHealth' entity game)

			-- Ship shield values
			shieldBarWidth       = boundingBoxWidth - (lostShieldPercentage * boundingBoxWidth)
			lostShield           = entity^.entityData.shipDamage.damageShield
			shipTotalShield      = (fromJust $ Map.lookup (entity^.entityData^.shipConfiguration^.shipConfigurationShipClass) 
				(game^.gameBuilder^.gbShipClasses))^.shipClassMaxDamage^.damageShield
			currentShield        = shipTotalShield - lostShield
			lostShieldPercentage = fromIntegral lostShield / fromIntegral shipTotalShield
			shieldPercentage     = fromIntegral currentShield / fromIntegral shipTotalShield
			-- Colour for the shields
			shieldBlue           = makeColor8 0 0 99 100

drawSelectionArc :: Float -> Float -> Picture
drawSelectionArc radius time = color (selectionColour time) $ rotate (time * 10) $ circle where
		circle = Pictures $ map (\x -> (ThickArc x (x + arcLength) radius) arcThickness) 
			[0, arcLength*2..(360 - arcLength*2)]
		selectionColour time    = (makeColor8 red (pulsingColour greenBase time) blue alpha)
		pulsingColour base time = (base + (round (oscLimit * (sin $ 2 * time))))
		arcThickness = 0.8
		arcLength    = 10
		greenBase    = (255 - (round oscLimit)) -- Minimum amount of green so the pulsing doesn't exceed max (255)
		oscLimit     = 35
		red          = 100
		blue         = 100
		alpha        = 75

healthColor :: Float -> Color 
healthColor health 
	| health <= rBoundary = (makeColor8 255 0 0 alpha)
	| health <= yBoundary = (makeColor8 255 (greenRatio health) 0 alpha)
	| otherwise           = (makeColor8 (redRatio health) 255 0 alpha)
	where
		greenRatio health = floor ((health - rBoundary)/(yBoundary - rBoundary) * 255)
		redRatio health   = 255 - floor ((health - yBoundary)/(gBoundary - yBoundary) * 255)
		alpha             = 100
		rBoundary         = 0.2
		yBoundary         = 0.5
		gBoundary         = 1

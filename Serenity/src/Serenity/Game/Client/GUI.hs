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
import Serenity.Sheen.Util

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import GHC.Float

isSelectedShip :: UIState ClientState -> (Entity Ship) -> (Bool, Bool)
isSelectedShip uiState entity = case uiState^.uiStateSelected of
	SelectionOwnShips ids -> (elem (entity^.entityID) ids, True)
	SelectionEnemyShips ids -> (elem (entity^.entityID) ids, False)
	_ -> (False, False)

isSelectedPlanet :: UIState ClientState -> Int -> Bool
isSelectedPlanet uiState planetID = case uiState^.uiStateSelected of
	SelectionPlanet p -> p == planetID
	_ -> False

handleMessage :: GUICommand -> UIState ClientState -> UIState ClientState
--handleMessage (ClientScroll (dx, dy)) uiState@UIState{ viewport=((x, y), z) } = uiState { viewport = ((x+dx, y+dy), z) }
--handleMessage (ClientZoom dz) uiState@UIState{ viewport=(loc, z) } = uiState { viewport = (loc, z+dz) }
handleMessage _ uiState = uiState

render :: Game -> UIState ClientState -> Assets -> Picture
render game uiState assets = Pictures
	[	translate 80 80 $ scale 0.9 0.9 $ parallaxShift 150 stars
	,	 translate 100 100 $ Pictures [parallaxShift 10 blueNebula, parallaxShift 7 greenNebula]
	,	(drawWorldToWindow. renderInWorld) game
	]
	where
		greenNebula       = getPicture "greenNebulaLayer" assets
		blueNebula        = getPicture "blueNebulaLayer" assets
		stars             = getPicture "starBackdropLayer" assets 
		drawWorldToWindow x = 
			scale (double2Float (s)) (double2Float (s)) $ 
			translate (double2Float (-vpx)) (double2Float (- vpy)) x
		
		parallaxShift a x = 
			scale (double2Float (1+s/100)) (double2Float (1+s/100)) $ 
			translate (double2Float (-vpx/a)) (double2Float (- vpy/a)) x

		(ww, wh)          = (fromIntegral w, fromIntegral h) where (w, h) = windowSize
		((vpx, vpy), vpz) = uiState^.uiStateViewport
		(gw, gh)          = game^.gameBuilder^.gbSector.sectorSize
		normScale         = ((min ww wh) / (max gw gh))
		s = vpz * normScale	

		renderInWorld game = pictures
			[	pictures $ map (pictureSpaceLane game uiState assets) $ game^.gameBuilder^.gbSector.sectorSpaceLanes
			,	pictures $ map (picturePlanet  game uiState assets) $ Map.toList $ game^.gameBuilder^.gbSector.sectorPlanets
			,	pictures $ map (pictureEntity game uiState assets (game^.gameTime)) $ Map.elems $ game^.gameShips
			]

picturePlanet :: Game -> UIState ClientState -> Assets -> (Int, Planet) -> Picture
picturePlanet game uiState assets (planetID, planet) = translate x y $ Pictures $ [p, name] ++ selectBox where
	planetSelected = isSelectedPlanet uiState planetID
	selectBox = if planetSelected then [planetSelectionArc 16.5 (double2Float (game^.gameTime))] else []
	(x,y) = pDouble2Float $ planet^.planetLocation
	p = getPictureSized (planet^.planetEcotype.ecotypeAssetName) 32 32 assets
	name = if planetSelected
		then color (greyN 0.7) $ translate (13) (-15) $ scale 0.03 0.03 $ Text (planet^.planetName)
		else color (greyN 0.7) $ translate (11) (-13) $ scale 0.02 0.02 $ Text (planet^.planetName)

pictureSpaceLane :: Game -> UIState ClientState -> Assets -> (Int, Int) -> Picture
pictureSpaceLane game uiState assets (p1, p2) = 
	color (changeAlpha green 0.5) 
	$ line $ map (\p -> pDouble2Float $ p^.planetLocation) planets where
		planets    = catMaybes $ map (\k -> Map.lookup k planetsMap) [p1, p2]
		planetsMap = game^.gameBuilder^.gbSector.sectorPlanets

pictureEntity :: Game -> UIState ClientState -> Assets -> Double -> Entity Ship -> Picture
pictureEntity game uiState assets time entity = pictures $ (shipAndHealth time) ++ beams where
	beams = concatMap pictureBeam (concat . Map.elems $ entity^.entityData.shipTargets)
	pictureBeam target = case Map.lookup target (game^.gameShips) of
		Just entity -> [color red $ line [(x, y + 2),
			(pDouble2Float $ entity^.entityData.shipLocation)]]
		Nothing -> []

	(shipIsSelected, shipSelectedFriendly) = isSelectedShip uiState entity
	selection      = 
		if shipIsSelected 
		then [ (if shipSelectedFriendly then drawSelectionArcGreen else drawSelectionArcRed) 6 (double2Float time)] 
		else []
	
	shipAndHealth time = map (translate x y) $
		selection ++ 
		[	rotate ((atan2 dx dy)/pi * 180) $ Pictures [shipBridge, (getPictureSized "transparent" dim dim assets)]
		,	(translate (-boundingBoxWidth / 2) 7 $ Pictures [boundingBox , healthMeter])
		,	(translate (-boundingBoxWidth / 2) 8 $ Pictures [boundingBox, shieldMeter])
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
	shieldBlue           = makeColor8 40 100 255 180

drawSelectionArcGreen :: Float -> Float -> Picture
drawSelectionArcGreen radius time = color (selectionColour time) $ rotate (time * 10) $ circle where
		circle = Pictures $ map (\x -> (ThickArc x (x + arcLength) radius) arcThickness) 
			[0, arcLength*2..(360 - arcLength*2)]
		selectionColour time    = (makeColor8 red (pulsingColour greenBase time) blue alpha)
		pulsingColour base time = (base + (round (oscLimit * (sin $ 2 * time))))
		arcThickness = 0.8
		arcLength    = 10
		greenBase    = (255 - (round oscLimit)) -- Minimum amount of green so the pulsing doesn't exceed max (255)
		oscLimit     = 15
		red          = 100
		blue         = 100
		alpha        = 180

drawSelectionArcRed :: Float -> Float -> Picture
drawSelectionArcRed radius time = color (selectionColour time) $ rotate (time * 10) $ circle where
		circle = Pictures $ map (\x -> (ThickArc x (x + arcLength) radius) arcThickness) 
			[0, arcLength*2..(360 - arcLength*2)]
		selectionColour time    = (makeColor8 (pulsingColour redBase time) green blue alpha)
		pulsingColour base time = (base + (round (oscLimit * (sin $ 2 * time))))
		arcThickness = 0.8
		arcLength    = 10
		redBase      = (255 - (round oscLimit)) -- Minimum amount of green so the pulsing doesn't exceed max (255)
		oscLimit     = 15
		green        = 30
		blue         = 30
		alpha        = 220

planetSelectionArc :: Float -> Float -> Picture
planetSelectionArc radius time = color (selectionColour time) $ rotate (time * 10) $ circle where
		circle = Pictures $ map (\x -> (ThickArc x (x + arcLength) radius) arcThickness) 
			[0, arcLength*2..(360 - arcLength*2)]
		selectionColour time    = (makeColor8 (pulsingColour redBase time) green blue alpha)
		pulsingColour base time = (base + (round (oscLimit * (sin $ 2 * time))))
		arcThickness = 0.8
		arcLength    = 10
		redBase      = (255 - (round oscLimit)) -- Minimum amount of green so the pulsing doesn't exceed max (255)
		oscLimit     = 15
		green        = 255
		blue         = 255
		alpha        = 220

healthColor :: Float -> Color 
healthColor health 
	| health <= rBoundary = (makeColor8 255 0 0 alpha)
	| health <= yBoundary = (makeColor8 255 (greenRatio health) 0 alpha)
	| otherwise           = (makeColor8 (redRatio health) 255 0 alpha)
	where
		greenRatio health = floor ((health - rBoundary)/(yBoundary - rBoundary) * 255)
		redRatio health   = 255 - floor ((health - yBoundary)/(gBoundary - yBoundary) * 255)
		alpha             = 180
		rBoundary         = 0.2
		yBoundary         = 0.5
		gBoundary         = 1

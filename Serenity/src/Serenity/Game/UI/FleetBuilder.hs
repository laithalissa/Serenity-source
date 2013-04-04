{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.FleetBuilder where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Model

import Control.Lens

data FleetData a = FleetData
	{	_fbTime                  :: Float
	,	_fbLoadButton            :: Button a ApplicationMode
	,	_fbSaveButton            :: Button a ApplicationMode
	,	_fbBackButton            :: Button a ApplicationMode
	,	_fbSquadrons             :: [String]
	,	_fbShowingShipAddBox     :: Bool
	,	_fbAddShipButton         :: Button a Bool
	,	_fbRemoveShipButton      :: Button a a
	,	_fbShowingSquadronAddBox :: Bool
	,	_fbAddSquadronButton     :: Button a Bool
	,	_fbRemoveSquadronButton  :: Button a (FleetData a)
	,	_fbShipSelectedIndex     :: Int
	,	_fbSquadronSelectedIndex :: Int
	,	_fbSlotSelectedIndex     :: Int
	}
makeLenses ''FleetData

class AppState a => FleetState a where
	aFleetB :: Simple Lens a (FleetData a)
	aFleet  :: Simple Lens a Fleet

initFBButton string action = 
	initButton 
	(initLabel (StaticString string) buttonColor (Just buttonBackground)       ) {_labelScale = 1.1}
	(initLabel (StaticString string) buttonColor (Just buttonPressedBackground)) {_labelScale = 1.1}
	[(ButtonEventMouseUpInside LeftButton (Modifiers Up Up Up), action)]

removeElement i list = map fst $ filter (\x -> x^._2 /= i) $ zip list [0..]

removeSelectedShip a = aFleet.fleetShips %~ (removeElement $ a^.aFleetB^.fbShipSelectedIndex) $ a
removeSelectedSquadron fbData = fbSquadrons %~ (removeElement $ fbData^.fbSquadronSelectedIndex) $ fbData

initFleetData :: FleetState a => Assets -> FleetData a
initFleetData assets = FleetData
	{	_fbTime                  = 0
	,	_fbLoadButton            = initMenuButton "Load      <<"  id
	,	_fbSaveButton            = initMenuButton "Save      >>"  id
	,	_fbBackButton            = initMenuButton "<-      Back" (\_ -> Menu)
	,	_fbSquadrons             = []
	,	_fbShowingShipAddBox     = False
	,	_fbAddShipButton         = initFBButton "+" (\_ -> True)
	,	_fbRemoveShipButton      = initFBButton "-" removeSelectedShip
	,	_fbShowingSquadronAddBox = False
	,	_fbAddSquadronButton     = initFBButton "+" (\_ -> True)
	,	_fbRemoveSquadronButton  = initFBButton "-" removeSelectedSquadron
	,	_fbShipSelectedIndex     = 0
	,	_fbSquadronSelectedIndex = 0
	,	_fbSlotSelectedIndex     = 0
	}

viewFleet :: FleetState a => a -> View a
viewFleet a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background2 (a^.aAssets) (a^.aFleetB.fbTime)
	}	<++
	[	-- Sidebar
		(initBox ((680, 0), (345, 750))) <++
		--[	button a (aFleetB.fbLoadButton) aMode ((80,450),(185,28))
		--,	button a (aFleetB.fbSaveButton) aMode ((80,350),(185,28))
		[	button a (aFleetB.fbBackButton) aMode ((80, 50),(185,28))
		]
	,	-- Ship list
		(initBox ((10, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[
		]
	,	-- Ship Class
		(initBox ((15+boxWidth, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[	
		]
	,	-- Squadron list
		(initBox ((20+boxWidth+boxWidth, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[	
		]
	,	-- Ship bar
		(initBox ((10, 750-boxHeight-45), (boxWidth+boxWidth, 35))) <++
		[	button a (aFleetB.fbAddShipButton) (aFleetB.fbShowingShipAddBox) ((7, 7),(24,20))
		,	button a (aFleetB.fbRemoveShipButton) id ((33, 7),(24,20))
		]
	,	-- Squadron bar
		(initBox ((10+boxWidth+boxWidth+10, 750-boxHeight-45), (boxWidth-5, 35))) <++
		[	button a (aFleetB.fbAddSquadronButton) (aFleetB.fbShowingSquadronAddBox) ((7, 7),(24,20))
		,	button a (aFleetB.fbRemoveSquadronButton) aFleetB ((33, 7),(24,20))
		]
	,	-- Slot Master list
		(initBox ((10, 0), (boxWidth-5, boxHeight))) <++
		[
		]
	,	-- Slot Detail list
		(initBox ((15+boxWidth, 0), (boxWidth-5, boxHeight))) <++
		[
		]
	,	-- Overall stats box
		(initBox ((20+boxWidth+boxWidth, 0), (boxWidth-5, boxHeight))) <++
		[
		]
	]

boxHeight = 210
boxWidth  = 218

background2 assets time = Just $ translate 500 250 $ Pictures 
	[	translate 80 80 $ scale 0.9 0.9 $ parallaxShift 150 stars
	,	translate 100 100 $ Pictures [parallaxShift 10 blueNebula, parallaxShift 7 greenNebula]
	]
	where
		greenNebula       = getPicture "greenNebulaLayer" assets
		blueNebula        = getPicture "blueNebulaLayer" assets
		stars             = getPicture "starBackdropLayer" assets
		
		parallaxShift a = translate (-vpx/a) (- vpy/a)
		(vpx, vpy) = (distance * (cos (time*speed)), distance * (sin (time*speed)))
		distance = 600
		speed = 0.16

timeFleet :: FleetState a => Float -> a -> a
timeFleet dt = aFleetB.fbTime +~ dt
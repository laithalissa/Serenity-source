{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.FleetBuilder where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Model

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Map (Map, keys)

data FleetData a = FleetData
	{	_fbTime                  :: Float
	,	_fbLoadButton            :: Button a ApplicationMode
	,	_fbSaveButton            :: Button a ApplicationMode
	,	_fbBackButton            :: Button a ApplicationMode
	,	_fbSquadrons             :: [String]
	,	_fbFleetNameTextBox      :: TextBoxLabel a
	,	_fbShowingShipAddBox     :: Bool
	,	_fbAddShipButton         :: Button a Bool
	,	_fbRemoveShipButton      :: Button a a
	,	_fbShipNameTextBox       :: TextBox a
	,	_fbShowingSquadronAddBox :: Bool
	,	_fbAddSquadronButton     :: Button a Bool
	,	_fbRemoveSquadronButton  :: Button a (FleetData a)
	,	_fbShipSelectedIndex     :: Int
	,	_fbClassSelectedIndex    :: Int
	,	_fbSquadronSelectedIndex :: Int
	,	_fbSlotSelectedIndex     :: Int
	,	_fbShipTable             :: Table a
	,	_fbClassTable            :: Table a
	}
makeLenses ''FleetData

class AppState a => FleetState a where
	aFleetB      :: Simple Lens a (FleetData a)
	aFleet       :: Simple Lens a Fleet
	aShipClasses :: Simple Lens a (Map String ShipClass)
	aWeapons     :: Simple Lens a (Map String Weapon)
	aSystems     :: Simple Lens a (Map String System)

initFBButton string action = 
	initButton 
	(initLabel (StaticString string) buttonColor (Just buttonBackground)       ) {_labelScale = 1.1}
	(initLabel (StaticString string) buttonColor (Just buttonPressedBackground)) {_labelScale = 1.1}
	[(ButtonEventMouseUpInside LeftButton (Modifiers Up Up Up), action)]

removeElement i list = map fst $ filter (\x -> snd x /= i) $ zip list [0..]

removeSelectedShip :: FleetState a => a -> a
removeSelectedShip = execState $ do 
	selected <- use $ aFleetB.fbShipSelectedIndex
	ships <- aFleet.fleetShips <%= (removeElement $ selected)
	when (length ships <= selected) $ aFleetB.fbShipSelectedIndex -= 1

removeSelectedSquadron fbData = fbSquadrons %~ (removeElement $ fbData^.fbSquadronSelectedIndex) $ fbData

initFleetData :: FleetState a => Assets -> FleetData a
initFleetData assets = FleetData
	{	_fbTime                  = 0
	,	_fbLoadButton            = initMenuButton "Load      <<"  id
	,	_fbSaveButton            = initMenuButton "Save      >>"  id
	,	_fbBackButton            = initMenuButton "<-      Back" (\_ -> Menu)
	,	_fbSquadrons             = []
	,	_fbFleetNameTextBox      = (initMenuTextBoxLabel "Fleet:") & (tblPostEdit .~ fileNameValidation)
	,	_fbShowingShipAddBox     = False
	,	_fbAddShipButton         = initFBButton "+" (\_ -> True)
	,	_fbRemoveShipButton      = initFBButton "-" removeSelectedShip
	,	_fbShipNameTextBox       = initMenuTextBox & (tbScale .~ 1.1)
	,	_fbShowingSquadronAddBox = False
	,	_fbAddSquadronButton     = initFBButton "+" (\_ -> True)
	,	_fbRemoveSquadronButton  = initFBButton "-" removeSelectedSquadron
	,	_fbShipSelectedIndex     = 0
	,	_fbClassSelectedIndex    = 0
	,	_fbSquadronSelectedIndex = 0
	,	_fbSlotSelectedIndex     = 0
	,	_fbShipTable             = initTable 20 Nothing
	,	_fbClassTable             = initTable 20 Nothing
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
		,	textBoxLabel a (aFleetB.fbFleetNameTextBox) (aFleet.fleetName) ((20,700),(305,28)) 65
		]
	,	-- Ship list
		(initBox ((10, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[	table a (aFleetB.fbShipTable) (to shipTableObjects) (shipTableViews a) ((0,0), (boxWidth-5, boxHeight))
		]
	,	-- Ship Class
		(initBox ((15+boxWidth, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[	table a (aFleetB.fbClassTable) (to classStringTableObjects) (classStringTableViews a) ((0,0), (boxWidth-5, boxHeight))
		]
	,	-- Squadron list
		(initBox ((20+boxWidth+boxWidth, 750-boxHeight), (boxWidth-5, boxHeight))) <++
		[	
		]
	,	-- Ship bar
		(initBox ((10, 750-boxHeight-45), (boxWidth+boxWidth, 35))) <++
		[	button a (aFleetB.fbAddShipButton) (aFleetB.fbShowingShipAddBox) ((7, 7),(24,20))
		,	button a (aFleetB.fbRemoveShipButton) id ((33, 7),(24,20))
		,	textBox a (aFleetB.fbShipNameTextBox) selectedLens ((66,7),(boxWidth+boxWidth-70,20))
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
	where
		selected = a^.aFleetB.fbShipSelectedIndex :: Int
		selectedLens :: FleetState a => Simple Lens a String
		selectedLens = aFleet.fleetShips.ixx selected.shipConfigurationShipName

boxHeight = 210
boxWidth  = 218

aClassNames :: FleetState a => Getter a [String]
aClassNames = to (\a -> reverse.sort $ keys $ a^.aShipClasses)

shipTableObjects :: FleetState a => a -> [(Int, ShipConfiguration)]
shipTableObjects a = zip [0..] $ a^.aFleet.fleetShips

shipTableViews :: FleetState a => a -> (Int, ShipConfiguration) -> View a
shipTableViews a (i, conf) = 
	(initBox ((2,0),(boxWidth-9,18))) <++
		[	labelStatic a 
				(	(initLabel (StaticString $ text) textColor Nothing) 
					& (labelScale .~ 0.9) 
					& (labelTextOffset .~ (3,4))
				)
				((0,0), (boxWidth-9,18))
			& (viewBackground .~ background)
			& (viewEventHandler .~ (Just eventHandler))
		]
	where
		text = conf^.shipConfigurationShipClass ++ ": " ++ conf^.shipConfigurationShipName
		selected = a^.aFleetB.fbShipSelectedIndex
		classIndex = fromMaybe 0 $ findIndex (conf^.shipConfigurationShipClass==) (a^.aClassNames)
		textColor = if i == selected then black else buttonBackground
		background = if i == selected then Just buttonBackground else Nothing
		eventHandler (UIEventMouseDownInside _ _ _) = flip execState a $ do 
			aFleetB.fbShipSelectedIndex .= i
			aFleetB.fbClassSelectedIndex .= classIndex
		eventHandler _ = a

classStringTableObjects :: FleetState a => a -> [(Int, String)]
classStringTableObjects a = zip [0..] $ a^.aClassNames

classStringTableViews :: FleetState a => a -> (Int, String) -> View a
classStringTableViews a (i, classString) = 
	(initBox ((2,0),(boxWidth-9,18))) <++
		[	labelStatic a 
				(	(initLabel (StaticString $ classString) textColor Nothing) 
					& (labelScale .~ 0.9) 
					& (labelTextOffset .~ (3,4))
				)
				((0,0), (boxWidth-9,18))
			& (viewBackground .~ background)
			& (viewEventHandler .~ (Just eventHandler))
		]
	where
		selected = a^.aFleetB.fbClassSelectedIndex
		shipSelected = a^.aFleetB.fbShipSelectedIndex
		textColor = if i == selected then black else buttonBackground
		background = if i == selected then Just buttonBackground else Nothing
		eventHandler (UIEventMouseDownInside _ _ _) = flip execState a $ do 
			aFleetB.fbClassSelectedIndex .= i
			aFleet.fleetShips.ixx shipSelected.shipConfigurationShipClass .= classString
		eventHandler _ = a

-- | Moving parallax background
background2 assets time = Just $ translate 500 250 $ Pictures 
	[	translate 80 80 $ scale 0.9 0.9 $ parallaxShift 150 stars
	,	translate 100 100 $ Pictures [parallaxShift 10 blueNebula, parallaxShift 7 greenNebula]
	]
	where
		greenNebula = getPicture "greenNebulaLayer" assets
		blueNebula  = getPicture "blueNebulaLayer" assets
		stars       = getPicture "starBackdropLayer" assets
		parallaxShift a = translate (-vpx/a) (- vpy/a)
		(vpx, vpy) = (distance * (cos (time*speed)), distance * (sin (time*speed)))
		distance = 600
		speed = 0.16

timeFleet :: FleetState a => Float -> a -> a
timeFleet dt = aFleetB.fbTime +~ dt

-- Utility

ixx :: Int -> Simple Lens [a] a
ixx i = lens (!!i) (\list x -> replaceAtIndex i x list)
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls
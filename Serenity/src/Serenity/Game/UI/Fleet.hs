{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Fleet where

import Serenity.External
import Serenity.Game.UI.Application
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.Color
import Serenity.Model.Game
import Serenity.Model.Message
import Serenity.Model.Ship
import Serenity.Network.Transport
import Serenity.Sheen

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data FleetStatus = Choosing | Submitting | Submitted deriving (Eq, Show)

data FleetData a = FleetData
	{	_fleetTitleLabel :: Label a
	,	_fleetDoneButton :: Button a FleetStatus
	,	_fleetStatus     :: FleetStatus
	,	_fleetChoice     :: Map String String
	,	_fleetShipsTable :: Table a
	}
makeLenses ''FleetData

class AppState a => FleetState a where
	aFleet :: Simple Lens a (FleetData a)
	aClientState :: Simple Lens a (Maybe ClientState)

initFleetData :: FleetState a => Assets -> FleetData a
initFleetData assets = FleetData
	{	_fleetTitleLabel = (initLabel (StaticString "Select your fleet") (bright green) Nothing) {_labelScale = 4}
	,	_fleetDoneButton = (initMenuButton "Done      ->" chooseFleet) & (buttonEnabled .~ fleetButtonEnabled)
	,	_fleetStatus = Choosing
	,	_fleetChoice = M.empty
	,	_fleetShipsTable = initTable 3 305 Nothing
	}

fleetButtonEnabled :: FleetState a => a -> Bool
fleetButtonEnabled a = (a^.aFleet.fleetStatus) == Choosing

chooseFleet :: FleetStatus -> FleetStatus
chooseFleet _ = Submitting

viewFleet :: FleetState a => a -> View a
viewFleet a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	label a (aFleet.fleetTitleLabel) ((30,685),(220,30))
	,	(initBox ((20, 50), (984, 625))) <++ -- Main
		[	table a (aFleet.fleetShipsTable) (to $ \_ -> shipClasses) (shipView a) ((5, 5), (974, 615))
		]
	,	button a (aFleet.fleetDoneButton) (aFleet.fleetStatus) ((819, 11), (185, 28))
	]
	where
		mClientState = a^.aClientState
		shipClasses = case mClientState of
			Just clientState -> take 6 $ M.toList $ clientState^.clientGame.gameBuilder.gbShipClasses
			Nothing -> []

shipView :: FleetState a => a -> (String, ShipClass) -> View a
shipView a (ship, shipClass) = initView ((0, 0), (300, 300)) <++
	[	labelStatic a (initLabel (StaticString ship) white Nothing & labelScale .~ 2) ((0, 265), (0, 0))
	,	pictureViewStatic a (initPictureView (StaticPicture (pictureShipClass (shipClass^.shipClassAssetName) ownerID (a^.aAssets)))) ((0, 125), (0, 0))
	-- ,	textBoxLabel a (to $ \_ -> countTextBox) (shipClassCount ship) ((0, 0), (300, 25)) 300
	]
	where
		mClientState = a^.aClientState
		ownerID = case mClientState of
			Just clientState -> clientState^.clientOwnerID
			Nothing -> 1
		--countTextBox = initMenuTextBoxLabel ("# of " ++ ship) (shipClassCount ship)

{-
shipClassCount :: FleetState a => String -> Simple Lens a String
shipClassCount ship = lens (aFleet^.fleetChoice.(at ship).(to $ fromMaybe "0")) (\a v -> aFleet.fleetChoice.(at ship).traverse .~ v $ a)
-}

pictureShipClass :: String -> Int -> Assets -> Picture
pictureShipClass name ownerID assets = translate 100 75 $ scale 12 12 $ rotate 90 $ Pictures [shipBridge, scale 0.015 0.015 $ getPicture name assets] where
	shipBridge = 
		translate (-0.052 * dim) (-0.47 * dim) 
		$ scale (0.105 * dim) (0.96 * dim) 
		$ color (ownerIDColor ownerID) 
		$ polygon [(0,0), (0,0.95), (0.5, 1), (1, 0.95), (1, 0)]
	dim = 10

timeFleet :: FleetState a => Float -> a -> a
timeFleet _ = execState $ do
	status <- use (aFleet.fleetStatus)
	if status == Submitted
		then aMode .= Play
		else return ()

timeFleetIO :: FleetState a => Float -> StateT a IO ()
timeFleetIO _ = do
	a <- get
	case a^.aFleet.fleetStatus of
		Submitting -> submitFleet
		_ -> return ()
	where
	submitFleet = do
		mClientState <- use aClientState
		case mClientState of
			Just clientState -> do
				let channels = clientState^.clientChannels
				fleet <- use (aFleet.fleetChoice)
				-- atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlSetFleet fleet)
				liftIO $ atomically $ writeTChan (channelOutbox channels) (ControlMessage $ ControlReady)
				aFleet.fleetStatus .= Submitted
			Nothing -> return ()

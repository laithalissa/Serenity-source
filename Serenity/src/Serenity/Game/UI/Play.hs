{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Play where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Minimap
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Game.Client.GUI
import Serenity.Game.Client.Main
import Serenity.Game.Client.Color
import Serenity.Model
import Serenity.Maths.Util

import Graphics.Gloss.Data.Extent
import Control.Lens
import Control.Monad.State
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map

data PlayData a = PlayData 
	{	_playSelectBox :: Maybe ((Float, Float), (Float, Float))
	}
makeLenses ''PlayData

class AppState a => PlayState a where
	aPlay :: Simple Lens a (PlayData a)
	aClientState :: Simple Lens a (Maybe ClientState)
	aName :: Simple Lens a String

initPlayData :: PlayState a => Assets -> PlayData a
initPlayData assets = PlayData
	{	_playSelectBox = Nothing
	}

viewPlay :: PlayState a => a -> View a
viewPlay a = case (a^.aClientState) of
	Just clientState -> (initView ((0,0),(1024, 750))) <++
		[	mainView a clientState
		,	sidebarView a clientState
		]
	Nothing -> mempty

sidebarView :: PlayState a => a -> ClientState -> View a
sidebarView a clientState = (initBox ((0,0),(200,750))) <++
	[	minimap a (aClientState.(to fromJust).clientGame) (a^.aClientState.(to fromJust).clientOwnerID) & (viewOrigin .~ (0,25))
	,	(initView ((0,0),(200,35))) & (viewBackground .~ (Just black)) <++
		[	labelStatic a ((initLabel (DynamicString aName) (bright myColor) Nothing) & (labelScale .~ 1)) ((5,5), (200,25))
		]
	,	(initView ((0,600),(200,150))) 
			& (viewBackground .~ (Just $ black))
			& (viewDepictMode .~ ViewDepictModeViewUppermost)
			& (viewDepict .~ (Just $ translate 100 75 $ pictures [background, scale s s $ foreground]))
	,	(initBox ((0,550),(200,50))) <++
		[	labelStatic a ((initLabel (StaticString name) (bright textColor) Nothing) & (labelScale .~ 1.2)) ((10,15), (200,50))
		]
	] where
		(game, uiState, assets) = (clientState^.clientGame, clientState^.clientUIState, a^.aAssets)
		background = getPictureSized "starBackdropSidebar" 200 150 assets

		((foreground, name), textColor, s) = case (clientState^.clientUIState.uiStateSelected) of
			SelectionOwnShips   (shipID:_) -> (lookupShip shipID, bright green, 12)
			SelectionEnemyShips (shipID:_) -> (lookupShip shipID, bright red, 12)
			SelectionPlanet     planetID   -> (lookupPlanet planetID, dark white, 9)
			_                              -> ((pictures [], ""), black, 12)
		
		lookupShip shipID = case game^.gameShips.(at shipID) of
			Just ship -> (pictureEntity game uiState assets 0 ship, ship^.shipName)
			Nothing -> (pictures [], "")

		lookupPlanet planetID = case game^.gameBuilder.gbSector.sectorPlanets.(at planetID) of
			Just planet -> (picturePlanet game uiState assets (planetID, planet), planet^.planetName)
			Nothing -> (pictures [], "")

		shipName = entityData.shipConfiguration.shipConfigurationShipClass

		myColor = (ownerIDColor (clientState^.clientOwnerID))

mainView :: PlayState a => a -> ClientState -> View a
mainView a clientState = (initView ((0,0),(1024, 750)))
	& (viewSubviewMode .~ ViewSubviewModeKeep)
	& (viewDepict .~ (Just $ translate (1024/2) (750/2) $ render (clientState^.clientGame) (clientState^.clientUIState) (a^.aAssets)))
	& (viewEventHandler .~ (Just $ \event -> handleMainEvent event a))
	<++ catMaybes
	[	selectionBox (a^.aPlay.playSelectBox)
	]

selectionBox Nothing = Nothing
selectionBox (Just box@((x1,y1),(x2,y2))) = Just $ (initView $ fromExtent $ boxToExtent box)
	& (viewBackground .~ (Just (changeAlpha (greyN 0.1) 0.4)))
	& (viewDepict .~ (Just outline)) 
	where
		outline = color (changeAlpha white 0.4) $ lineLoop points
		points = [(0, 0),(0, max y1 y2 - (min y1 y2)),(max x1 x2 - (min x1 x2), max y1 y2 -(min y1 y2)),(max x1 x2 - (min x1 x2), 0)]

boxToExtent ((x1,y1),(x2,y2)) = makeExtent maxY minY maxX minX where
	minX = floor $ min x1 x2
	minY = floor $ min y1 y2
	maxX = floor $ max x1 x2
	maxY = floor $ max y1 y2

handleMainEvent :: PlayState a => UIEvent -> a -> a
handleMainEvent event = case event of
	UIEventMouseDownInside _ point mods -> startSelect point
	UIEventMouseUpInside   LeftButton point (Modifiers Up Up Up) -> endSelect False point
	UIEventMouseUpInside   LeftButton point (Modifiers Down Up Up) -> endSelect True point
	UIEventMouseUpInside   RightButton point mods -> endSelectRight point
	UIEventMouseUpOutside  LeftButton point mods -> endSelect False point
	UIEventMotion point -> continueSelect point
	UIEventKeyPress _ _ _ -> aClientState %~ handleGameEvent event
	_ -> id

handleGameEvent :: UIEvent -> Maybe ClientState -> Maybe ClientState
handleGameEvent event = case event of
	UIEventMouseUpInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Up mods point)
	UIEventMouseDownInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Down mods point)
	UIEventKeyPress key keystate mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey key keystate mods (0,0))
	_ -> fmap id

startSelect :: PlayState a => (Float, Float) -> a -> a
startSelect point =  aPlay.playSelectBox.~ Just (point, point)

continueSelect :: PlayState a => (Float, Float) -> a -> a
continueSelect point = aPlay.playSelectBox.traverse._2 .~ point

cancelSelect :: PlayState a => a -> a
cancelSelect = aPlay.playSelectBox .~ Nothing

endSelect :: PlayState a => Bool -> (Float, Float) -> a -> a
endSelect deselect point = execState $ do
	overMaybe (aPlay.playSelectBox) (aClientState.traverse) (select.boxToExtent)
	aPlay.playSelectBox .= Nothing 
	where
		select extent = execState $ do
			clientState <- get
			(friendly, enemy, planets, wasDrag) <- return $ lassoShips extent clientState
			newSelection <- return $ shipSelection friendly enemy planets
			when ((not $ selectionIsEmpty newSelection) || deselect) $ clientUIState.uiStateSelected .= newSelection

shipSelection [] [] (p:_) = SelectionPlanet p
shipSelection [] e  _     = SelectionEnemyShips e
shipSelection f  _  _     = SelectionOwnShips f

endSelectRight :: PlayState a => (Float, Float) -> a -> a
endSelectRight point = execState $ do
	overMaybe (aPlay.playSelectBox) (aClientState.traverse) order
	meh2 <- use $ aClientState
	aPlay.playSelectBox .= Nothing 
	where
		order box = execState $ do
			clientState <- get
			(friendly, enemy, planets, wasDrag) <- return $ lassoShips (boxToExtent box) clientState
			(f,e,p)  <- return $ selectionToTriple (clientState^.clientUIState.uiStateSelected)
			newOrder <- return $ chooseOrder friendly enemy planets (translatePoint clientState $ pFloat2Double $ fst $ box)
			clientCommands %= (++ if f == [] then [] else [GiveOrder (head f) newOrder])

		translatePoint clientState point = mapLocationFromView point (clientState^.clientUIState.uiStateViewport) (sX, sY) where
			(sX,sY) = clientState^.clientGame.gameBuilder.gbSector.sectorSize

chooseOrder [] [] []    loc = OrderMove loc Nothing
chooseOrder [] [] (p:_) loc = OrderCapture p
chooseOrder [] (e:_)  _ loc = OrderAttack e
chooseOrder (f:_)  _  _ loc = OrderGuardShip f

timePlay :: PlayState a => Float -> a -> a
timePlay _ = execState $ do
	mClientState <- use aClientState
	case mClientState of 
		Nothing -> aMode .= Menu
		Just clientState -> if (clientState^.clientGameStatus) == Complete
			then aMode .= End
			else return ()

timePlayIO :: PlayState a => Float -> StateT a IO ()
timePlayIO dt = do
	mClientState <- use aClientState
	case mClientState of 
		Just clientState -> do
			newClientState <- liftIO $ handleStep dt clientState
			aClientState .= Just newClientState
		Nothing -> return ()



picturePlanet :: Game -> UIState ClientState -> Assets -> (Int, Planet) -> Picture
picturePlanet game uiState assets (planetID, planet) = Pictures $ [p] where
	p = getPictureSized (planet^.planetEcotype.ecotypeAssetName) 15 15 assets

pictureEntity :: Game -> UIState ClientState -> Assets -> Double -> Entity Ship -> Picture
pictureEntity game uiState assets time entity = rotate 90 $ Pictures [shipBridge, (getPictureSized "transparent" dim dim assets)] where	
	shipBridge = 
		translate (-0.052 * dim) (-0.47 * dim) 
		$ scale (0.105 * dim) (0.96 * dim) 
		$ color (ownerIDColor (entity^.ownerID)) 
		$ polygon [(0,0), (0,0.95), (0.5, 1), (1, 0.95), (1, 0)]
	dim     = 10
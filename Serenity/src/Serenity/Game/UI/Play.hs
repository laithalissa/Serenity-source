{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Play where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Minimap
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Game.Client.GUI
import Serenity.Game.Client.Main

import Graphics.Gloss.Data.Extent
import Control.Lens
import Control.Monad.State
import Data.Monoid
import Data.Maybe

data PlayData a = PlayData 
	{	_playSelectBox :: Maybe ((Float, Float), (Float, Float))
	}
makeLenses ''PlayData

class AppState a => PlayState a where
	aPlay :: Simple Lens a (PlayData a)
	aClientState :: Simple Lens a (Maybe ClientState)

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
	[	minimap a (aClientState.(to fromJust).clientGame) (a^.aClientState.(to fromJust).clientOwnerID) & (viewOrigin .~ (0,550))
	]

mainView :: PlayState a => a -> ClientState -> View a
mainView a clientState = (initView ((0,0),(1024, 750)))
	& (viewSubviewMode .~ ViewSubviewModeKeep)
	& (viewDepict .~ (Just $ render (clientState^.clientGame) (clientState^.clientUIState) (a^.aAssets)))
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
	UIEventMouseDownInside LeftButton point mods -> startSelect point
	UIEventMouseUpInside   LeftButton point mods -> endSelect point
	UIEventMouseUpOutside  LeftButton point mods -> endSelect point
	UIEventMotion                     point      -> continueSelect point
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

endSelect :: PlayState a => (Float, Float) -> a -> a
endSelect point = execState $ do
	overMaybe (aPlay.playSelectBox) (aClientState.traverse) (select.boxToExtent)
	aPlay.playSelectBox .= Nothing where
		select extent = execState $ do
			clientState <- get
			ids <- return $ selectShips extent clientState
			clientUIState.uiStateSelected .= ids

cancelSelect :: PlayState a => a -> a
cancelSelect = aPlay.playSelectBox .~ Nothing

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
			channels <- return $ clientState^.clientChannels
			newClientState <- liftIO $ handleStep dt clientState
			aClientState .= Just newClientState
		Nothing -> return ()

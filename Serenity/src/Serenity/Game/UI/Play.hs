{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Play where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.UI.Minimap
import Serenity.Game.Client.ClientState
import Serenity.External
import Serenity.Model
import Serenity.Maths.Util
import Serenity.Game.Client.GUI
import Serenity.Game.Client.Main
import Serenity.Network.Transport

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
	& (viewDepict .~ (Just $ render (clientState^.clientGame) (clientState^.clientUIState) (a^.aAssets)))
	& (viewEventHandler .~ (Just $ \event -> aClientState %~ handleGameEvent event $ a))

handleGameEvent :: UIEvent -> Maybe ClientState -> Maybe ClientState
handleGameEvent event = case event of
	UIEventMouseUpInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Up mods point)
	UIEventMouseDownInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Down mods point)
	UIEventKeyPress key keystate mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey key keystate mods (0,0))
	_ -> fmap id

timePlay :: PlayState a => Float -> a -> a
timePlay _ = execState $ do
	mClientState <- use aClientState
	case mClientState of 
		Nothing -> aMode .= Menu
		Just _ -> return ()

timePlayIO :: PlayState a => Float -> StateT a IO ()
timePlayIO dt = do
	mClientState <- use aClientState
	case mClientState of 
		Just clientState -> do
			channels <- return $ clientState^.clientChannels
			newClientState <- liftIO $ handleStep dt clientState
			aClientState .= Just newClientState
		Nothing -> return ()

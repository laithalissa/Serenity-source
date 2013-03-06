{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Play where

import Serenity.Sheen
import Serenity.Game.UI.Application
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

data PlayData a = PlayData 

initPlayData :: Simple Lens a (PlayData a) -> Assets -> PlayData a
initPlayData aPlay aAssets = PlayData

viewPlay :: a -> Simple Lens a (PlayData a) -> Simple Lens a (Maybe ClientState) -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewPlay a aPlay aClientState aAssets aMode = case (a^.aClientState) of
	Just clientState -> (initView ((0,0),(1024, 750))) <++
		[	mainView a aClientState clientState (a^.aAssets)
		,	sidebarView clientState
		]
	Nothing -> mempty

sidebarView clientState = (initBox ((0,0),(200,750)))
	& (viewBackground .~ Just translucentBackground)

mainView :: a -> Simple Lens a (Maybe ClientState) -> ClientState -> Assets -> View a
mainView a aClientState clientState assets = (initView ((0,0),(1024, 750)))
	& (viewDepict .~ (Just $ render (clientState^.clientGame) (clientState^.clientUIState) assets))
	& (viewEventHandler .~ (Just $ \event -> aClientState %~ handleGameEvent event $ a))

handleGameEvent :: UIEvent -> Maybe ClientState -> Maybe ClientState
handleGameEvent event = case event of
		UIEventMouseUpInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Up mods point)
		UIEventMouseDownInside mouseButton point mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey (MouseButton mouseButton) Down mods point)
		UIEventKeyPress key keystate mods -> fmap $ Serenity.Game.Client.Main.handleEvent (EventKey key keystate mods (0,0))
		_ -> fmap id

timePlay :: Simple Lens a (PlayData a) -> Simple Lens a (Maybe ClientState) -> Simple Lens a ApplicationMode -> Float -> a -> a
timePlay _ aMClientState aMode _ = execState $ do
	mClientState <- use aMClientState
	case mClientState of 
		Nothing -> aMode .= Menu
		Just _ -> return ()

timePlayIO :: Simple Lens a (PlayData a) -> Simple Lens a (Maybe ClientState) -> Float -> StateT a IO ()
timePlayIO aPlay aClientState dt = do
	mClientState <- use aClientState
	case mClientState of 
		Just clientState -> do
			channels <- return $ clientState^.clientChannels
			newClientState <- liftIO $ handleStep dt clientState
			aClientState .= Just newClientState
		Nothing -> return ()

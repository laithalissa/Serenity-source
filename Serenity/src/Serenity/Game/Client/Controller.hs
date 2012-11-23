module Serenity.Game.Client.Controller
	( UIEvent(..)
	, initClientState
	, drawClientState
	, newClientStateFromEvent
	)
where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game (Event(..))

import Serenity.Game.Client.ClientState

import Serenity.Sheen.UIEvent
import Serenity.Sheen.View

initClientState :: ClientState
initClientState = ClientState [] mainView

drawClientState :: ClientState -> IO Picture
drawClientState clientState = do
	let views = drawView (uiState clientState) clientState
	return $ Translate (fromIntegral $ - 1024 `div` 2) (fromIntegral $ - 768 `div` 2) views

newClientStateFromEvent :: Event -> ClientState -> ClientState
newClientStateFromEvent event clientState = handleViewEvent event mainView clientState

mainView :: View ClientState
mainView = (makeView "main" (0, 1024, 0, 768))
	{ subviews = [menuView, gameView]
	, background = Just black
	}

menuView :: View ClientState
menuView = (makeView "menu" (0, 100, 0, 768))
	{ background = Just red
	}

gameView :: View ClientState
gameView = (makeView "game" (100, 1024, 0, 768))
	{ background = Just green
	, eventHandler = Just (\_ clientState ->
		clientState { uiState = changeView "game" (\v -> if background v == Just green
			then v { background = Just blue }
			else v { background = Just green }
		) (uiState clientState) }
	  )
	}


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.Views.Button where

import Serenity.Sheen.View
import Serenity.Sheen.UIEvent
import Serenity.Sheen.Views.Label

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import Data.Map (Map)
import qualified Data.Map as Map

data Button a b = Button
	{	_buttonLabel :: Label a
	,	_buttonLabelPushed :: Label a
	,	_buttonMode :: ButtonMode
	,	_buttonIsPushed :: Bool
	,	_buttonEnabled :: Bool
	,	_buttonAction :: Map ButtonEvent (b -> b)
	}

data ButtonMode = Momentary | Toggle

data ButtonEvent = ButtonEvent MouseButton KeyState Modifiers deriving (Show, Eq, Ord)

initButton label inLabel action = Button
	{	_buttonLabel = label
	,	_buttonLabelPushed = inLabel
	,	_buttonMode = Momentary
	,	_buttonIsPushed = False
	,	_buttonEnabled = True
	,	_buttonAction = Map.fromList action
	}

uiEvent2ButtonEvent (UIEventKey _ (MouseButton mouseButton) state mods) = Just $ ButtonEvent mouseButton state mods
uiEvent2ButtonEvent _ = Nothing

makeLenses ''ButtonMode
makeLenses ''Button

button :: a -> Simple Lens a (Button a b) -> Simple Lens a b -> ((Int, Int), (Int, Int)) -> View a
button a button lens bounds = (label a (button.labelToDisplay) bounds)
	{	_viewEventHandler = case a^.button.buttonEnabled of
		True  -> Just $ \event -> buttonEventHandler lens (uiEvent2ButtonEvent event) a
		False -> Nothing
	} where
		labelToDisplay :: Getter (Button a b) (Label a)
		labelToDisplay = if (a^.button.buttonEnabled)
			then if (a^.button.buttonIsPushed) then buttonLabelPushed else buttonLabel
			else to (\a -> labelBackground .~ (Just $ greyN 0.3) $ a^.buttonLabel)

		buttonEventHandler _ Nothing = id
		buttonEventHandler lens (Just event@(ButtonEvent _ keyState _)) = execState $
			do	modify $ \a -> flip fromMaybe (runButtonAction a) $ a
				modify $ \a -> button.buttonIsPushed %~ (updatePushed a) $ a
			where
			runButtonAction a = do
				action <- a^.button.buttonAction.(at event)
				return (lens %~ action $ a)
			updatePushed a isPushed = case a^.button.buttonMode of
				Momentary -> keyState == Down
				Toggle    -> not isPushed

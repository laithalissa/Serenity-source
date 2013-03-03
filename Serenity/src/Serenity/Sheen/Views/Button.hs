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

data ButtonEvent = 
	  ButtonEventMouseUpInside   MouseButton Modifiers 
	| ButtonEventMouseUpOutside  MouseButton Modifiers 
	| ButtonEventMouseDownInside MouseButton Modifiers 
	| ButtonEventMouseOverInside
	| ButtonEventMouseOverOutside
	deriving (Show, Eq, Ord)

uiEvent2ButtonEvent (UIEventMouseUpInside   mouseButton _ mods) = Just $ ButtonEventMouseUpInside   mouseButton mods
uiEvent2ButtonEvent (UIEventMouseDownInside mouseButton _ mods) = Just $ ButtonEventMouseDownInside mouseButton mods
uiEvent2ButtonEvent (UIEventMouseUpOutside  mouseButton _ mods) = Just $ ButtonEventMouseUpOutside  mouseButton mods
uiEvent2ButtonEvent UIEventMouseOverInside  = Just $ ButtonEventMouseOverInside
uiEvent2ButtonEvent UIEventMouseOverOutside = Just $ ButtonEventMouseOverOutside
uiEvent2ButtonEvent _ = Nothing

data Button a b = Button
	{	_buttonLabel :: Label a
	,	_buttonLabelPushed :: Label a
	,	_buttonMode :: ButtonMode
	,	_buttonIsPushed :: Bool
	,	_buttonIsPushedButMouseOutside :: Bool
	,	_buttonEnabled :: Bool
	,	_buttonAction :: Map ButtonEvent (b -> b)
	}

data ButtonMode = Momentary | Toggle

initButton label inLabel action = Button
	{	_buttonLabel = label
	,	_buttonLabelPushed = inLabel
	,	_buttonMode = Momentary
	,	_buttonIsPushed = False
	,	_buttonIsPushedButMouseOutside = False
	,	_buttonEnabled = True
	,	_buttonAction = Map.fromList action
	}

makeLenses ''ButtonMode
makeLenses ''Button

button :: a -> Simple Lens a (Button a b) -> Simple Lens a b -> ((Int, Int), (Int, Int)) -> View a
button a button lens bounds = (label a (button.labelToDisplay) bounds)
	{	_viewEventHandler = if a^.button.buttonEnabled
		then Just $ \event -> buttonEventHandler lens (uiEvent2ButtonEvent event) a
		else Nothing
	} where
		labelToDisplay :: Getter (Button a b) (Label a)
		labelToDisplay = if (a^.button.buttonEnabled)
			then if (a^.button.buttonIsPushed) then buttonLabelPushed else buttonLabel
			else to (\a -> labelBackground .~ (Just $ greyN 0.3) $ a^.buttonLabel)

		buttonEventHandler _ Nothing = id

		buttonEventHandler _ (Just (ButtonEventMouseDownInside _ _)) = execState $ do
			button.buttonIsPushed .= True

		buttonEventHandler lens (Just event@(ButtonEventMouseUpInside _ _)) = execState $ do
			modify $ flip fromMaybe (runButtonAction a)
			button.buttonIsPushed %= (updatePushed a)
			where
			runButtonAction a = do
				action <- a^.button.buttonAction.(at event)
				return (lens %~ action $ a)
			updatePushed a isPushed = case a^.button.buttonMode of
				Momentary -> False
				Toggle    -> not isPushed

		buttonEventHandler _ (Just (ButtonEventMouseUpOutside  _ _)) = execState $ do
			button.buttonIsPushedButMouseOutside .= False

		buttonEventHandler _ (Just ButtonEventMouseOverOutside) = execState $ do
			pushed <- use $ button.buttonIsPushed
			when pushed $ do 
				button.buttonIsPushedButMouseOutside .= True
				button.buttonIsPushed .= False

		buttonEventHandler _ (Just ButtonEventMouseOverInside) = execState $ do
			pushed <- use $ button.buttonIsPushedButMouseOutside
			when pushed $ do 
				button.buttonIsPushedButMouseOutside .= False
				button.buttonIsPushed .= True

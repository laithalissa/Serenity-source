{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module StateSwitcher
(	module Graphics.Gloss
,	module Graphics.Gloss.Interface.IO.Game
,	Application
,	Transition(..)
,	ScreenState(..)
,	Screen(..)
,	initApplication
,	update
,	handleInput
,	render
) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Gloss(Picture)
import Graphics.Gloss.Interface.IO.Game(Event)

-- | any screen needs to instance this class, when it has focus, these 3 functions will gain control
class Screen state where	
	stateUpdate 
		:: Float -- ^ seconds passed since this function was called last
		-> state -- ^ current state of screen
		-> Maybe (ScreenState, Transition) -- ^ return type allows:
		-- the current state to update itself by returning Just(updated-state-wrapper, Update),
		-- a transition to another state by Just (new-state-wrapper, Push),
		-- going back to previous state by Nothing
	stateRender :: state -> Picture
	stateInput :: Event -> state -> Maybe (ScreenState, Transition)
 
data Transition = Push | Update
data ScreenState = forall a. (Screen a) => ScreenState a

instance Screen ScreenState where
	stateUpdate delta (ScreenState s) = stateUpdate delta s
	stateRender (ScreenState s) = stateRender s
	stateInput event (ScreenState s) = stateInput event s

type Mode = Int

data Application = Application 
	{	_applicationMode :: Mode
	,	_applicationStates :: Map Mode ScreenState
	}
makeLenses ''Application

initApplication :: (Screen s) => s -> Application
initApplication s = Application { _applicationMode = 0, _applicationStates = ScreenState s }

update :: Float -> Application -> Application
update timeDelta switcher = transitionMatcher timeDelta switcher stateUpdate

handleInput :: Event -> Application -> Application
handleInput event switcher = transitionMatcher event switcher stateInput

transitionMatcher 
	:: a 
	-> Application 
	-> (a -> ScreenState -> Maybe(ScreenState, Transition))
	-> Application

transitionMatcher event switcher f =
	case f event (currentState switcher) of
		Nothing -> Application
			{ 	stack = tail $ stack switcher
			, 	currentState = head $ stack switcher 
			} 
		Just (sw2, Push) -> Application
			{ 	stack = (currentState switcher):(stack switcher) 
			, 	currentState = sw2 
			} 
		Just (sw2, Update) -> switcher{currentState=sw2}

render :: Application -> Picture
render = stateRender . currentState

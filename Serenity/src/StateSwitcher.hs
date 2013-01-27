{-# LANGUAGE ExistentialQuantification #-}
module StateSwitcher
(	Switcher
,	Transition(..)
,	StateWrapper(..)
,	AppState(..)
,	initialize
,	update
,	handleInput
,	render
,	wrap
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Interface.IO.Game(Event)

-- | any screen needs to instance this class, when it has focus, these 3 functions will gain control
class AppState state where	
	stateUpdate 
		:: Float -- ^ seconds passed since this function was called last
		-> state -- ^ current state of screen
		-> Maybe (StateWrapper, Transition)
	stateRender :: state -> Picture
	stateInput :: Event -> state -> Maybe (StateWrapper, Transition)
 
data Transition = Push | Update
data StateWrapper = forall a. (AppState a) => StateWrapper a

instance AppState StateWrapper where
	stateUpdate delta (StateWrapper s) = stateUpdate delta s
	stateRender (StateWrapper s) = stateRender s
	stateInput event (StateWrapper s) = stateInput event s

data Switcher = Switcher 
	{	stack :: [StateWrapper]
	,	currentState :: StateWrapper
	}

wrap :: (AppState s) => s -> StateWrapper
wrap s = StateWrapper s

initialize :: (AppState s) => s -> Switcher
initialize s = Switcher{stack = [], currentState = StateWrapper s}

update :: Float -> Switcher -> Switcher
update timeDelta switcher = transitionMatcher timeDelta switcher stateUpdate

handleInput :: Event -> Switcher -> Switcher
handleInput event switcher = transitionMatcher event switcher stateInput

transitionMatcher 
	:: a 
	-> Switcher 
	-> (a -> StateWrapper -> Maybe(StateWrapper, Transition))
	-> Switcher

transitionMatcher event switcher f =
	case (f event sw) of
		Nothing -> Switcher
			{ 	stack = tail $ stack switcher
			, 	currentState = head $ stack switcher 
			} 
		Just (sw2, Push) -> Switcher
			{ 	stack = (currentState switcher):(stack switcher) 
			, 	currentState = sw2 
			} 
		Just (sw2, Update) -> switcher{currentState=sw2}
	where
		sw = currentState switcher

render :: Switcher -> Picture
render = stateRender . currentState

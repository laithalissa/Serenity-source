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
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Interface.IO.Game(Event)

class AppState state where	
	stateUpdate :: state -> Float -> Maybe (StateWrapper, Transition)
	stateRender :: state -> Picture
	stateInput :: state -> Event -> Maybe (StateWrapper, Transition)
 
data Transition = Push | Update
data StateWrapper = forall a. (AppState a) => StateWrapper a

instance AppState StateWrapper where
	stateUpdate (StateWrapper s) = stateUpdate s
	stateRender (StateWrapper s) = stateRender s
	stateInput (StateWrapper s) = stateInput s

data Switcher = Switcher 
	{	stack :: [StateWrapper]
	,	currentState :: StateWrapper
	}

initialize :: (AppState s) => s -> Switcher
initialize s = Switcher{stack = [], currentState = StateWrapper s}

update :: Float -> Switcher -> Switcher
update timeDelta switcher = transitionMatcher timeDelta switcher stateUpdate

handleInput :: Event -> Switcher -> Switcher
handleInput event switcher = transitionMatcher event switcher stateInput

transitionMatcher 
	:: a 
	-> Switcher 
	-> (StateWrapper -> a -> Maybe(StateWrapper, Transition))
	-> Switcher

transitionMatcher event switcher f =
	case (f sw event) of
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

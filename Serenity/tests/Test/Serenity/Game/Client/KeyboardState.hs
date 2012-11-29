{-# LANGUAGE TemplateHaskell #-}

module Test.Serenity.Game.Client.KeyboardState
(	tests
) where

import Serenity.Game.Client.KeyboardState 

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import qualified Data.Sequence as Seq
import Data.Sequence (fromList)
import Graphics.Gloss.Interface.Pure.Game

import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)

$(derive makeArbitrary ''Key)
$(derive makeArbitrary ''KeyState)
$(derive makeArbitrary ''MouseButton)
$(derive makeArbitrary ''SpecialKey)

tests = testGroup "GameStateUpdate Tests"
	[	testProperty "Test filterOutOne removes a character if it's in the input string" testFilterOutFirst
	,	testProperty "Test handleKeyEvent adds or removes up tp one from the stack" testHandleKeyEvent
	,	testProperty "Test handleKeyEvent key up then down returns to initial state" testHandleKeyEventUpDown
	,	testCase "Test a simple sequence of key events are recorded correctly" testCorrectKeysAreDown
	,	testCase "Test whether querying a key that is down" testQueryAKey
	,	testCase "Test whether querying a key that is not down" testQueryWKey
	,	testCase "Test querying order of several keys" testQueryAWKeys
	]

someKeyEvents = 
	[	(Char 'a', Down )
	,	(Char 'a', Up   )
	,	(Char 'b', Down )
	,	(Char 'a', Down )
	,	(Char 'w', Down )
	,	(Char 'w', Up   )
	,	(Char 's', Down )
	]

aKeyboardState = foldl (\kstate (key, s) -> handleKeyEvent key s kstate) emptyKeyboardState someKeyEvents 

testCorrectKeysAreDown = aKeyboardState @?= (Seq.fromList [Char 's', Char 'a', Char 'b'])
testQueryAKey = isKeyDown aKeyboardState (Char 'a') @?= True
testQueryWKey = isKeyDown aKeyboardState (Char 'w') @?= False
testQueryAWKeys = keysDown aKeyboardState [Char 'a', Char 'w', Char 's'] @?= [Char 's', Char 'a']

testFilterOutFirst e s = length s - d == Seq.length (filterOutFirst (==e) (fromList s)) where
	d = if e `elem` s then 1 else 0
	types = (e :: Char, s :: String)

testHandleKeyEvent key event state = length state - (Seq.length $ handleKeyEvent key event (fromList state)) <= 1

testHandleKeyEventUpDown key state = (handleKeyEvent key Up $ handleKeyEvent key Down $ fromList state) == (fromList state)




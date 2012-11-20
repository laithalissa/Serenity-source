module Serenity.Game.Server.KeyboardState where

import Data.Set as Set
import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..), KeyState(..))

type KeyboardState = Set Key

initKeyboardState :: KeyboardState
initKeyboardState = Set.empty

handleKeyEvent :: Key -> KeyState -> KeyboardState -> KeyboardState
handleKeyEvent k Down = Set.insert k
handleKeyEvent k Up   = Set.delete k

isKeyDown :: KeyboardState -> Key -> Bool
isKeyDown kb k = Set.member k kb

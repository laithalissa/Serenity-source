module Serenity.Game.Client.KeyboardState 
(	KeyboardState
,	Key(..)
,	KeyState(..)
,	emptyKeyboardState
,	handleKeyEvent
,	isKeyDown
,	keysDown
,	filterOutFirst
,	keyMostRecentDownFrom
) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (<|), (><), ViewL(..))
import Data.Foldable (toList)

import Graphics.Gloss.Interface.Pure.Game (Key(..), KeyState(..))

type KeyboardState = Seq Key

emptyKeyboardState :: KeyboardState
emptyKeyboardState = Seq.empty

handleKeyEvent :: Key -> KeyState -> KeyboardState -> KeyboardState
handleKeyEvent k Down = (<|) k   
handleKeyEvent k Up   = filterOutFirst (==k) 

filterOutFirst :: (Eq a) => (a -> Bool) -> Seq a -> Seq a
filterOutFirst p xs = h >< t' where 
	(h, t) = Seq.breakl p xs
	t' = if t == Seq.empty then t else t''
	(_ :< t'') = Seq.viewl t

isKeyDown :: KeyboardState -> Key -> Bool
isKeyDown state c = not . ([]==) $ keysDown state [c]

keysDown :: KeyboardState -> [Key] -> [Key]
keysDown state keys = keysDown' state (Seq.fromList keys) [] where
	keysDown' state keys ans = if Seq.empty == keys || state == Seq.empty 
		then reverse ans 
		else if e `elem` (toList keys) 
			then keysDown' state' (filterOutFirst (==e) keys) (e:ans)
			else keysDown' state' keys ans
			where
				(e :< state') = Seq.viewl state

keyMostRecentDownFrom :: KeyboardState -> Key -> [Key] -> Bool
keyMostRecentDownFrom ks key keys = case topKey (keysDown ks keys) of
	Just tk -> key == tk 
	Nothing -> False 
	where
		topKey (x:xs) = Just x
		topKey [] = Nothing
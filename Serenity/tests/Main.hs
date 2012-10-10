module Main where

import Test.QuickCheck (quickCheck)
import Text.Printf

prop_reverseReverse :: [Char] -> Bool
prop_reverseReverse s = (reverse . reverse) s == s

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests = 
	[	("reverse.reverse/id", quickCheck prop_reverseReverse)
	,	("reverse.reverse/id", quickCheck prop_reverseReverse)
	]
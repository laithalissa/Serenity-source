module Test.Serenity.Maths.Util
(	tests
) where

import Serenity.Maths.Util

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

import Data.DeriveTH(derive)
import Data.Derive.Arbitrary(makeArbitrary)

tests = testGroup "Math Util Tests"
	[	testProperty "Test range is limited to within bounds with rangeLimitAttainBounds" testRangeLimitAttainBounds
	,	testProperty "Test range is limited to within bounds with testRangeLimitWrapReal" testRangeLimitWrapReal
	,	testProperty "Test range is limited to within bounds with testRangeLimitWrapInt" testRangeLimitWrapInt
	]

(===>) p q = not p || q

testRangeLimitAttainBounds minx maxx x = maxx > minx ==>
	(minx <= x') && 
	(x' <= maxx) && 
	((x < minx)===>(x'==minx)) && 
	((x > maxx)===>(x'==maxx)) 
	where
		x' = rangeLimitAttainBounds minx maxx x
		types = x :: Int

testRangeLimitWrapReal minx maxx x = maxx > minx ==>
	(minx <= x') && (x' <= maxx) 
	where
		x' = rangeLimitWrapReal minx maxx x
		types = x :: Double

testRangeLimitWrapInt minx maxx x = (maxx > minx) ==>
	(minx <= x') && (x' <= maxx)
	where
		x' = rangeLimitWrapInt minx maxx x
		types = x :: Integer

testRangeCheck minx maxx x = maxx > minx ==> case x' of
	Right x'' -> (minx <= x'') && (x'' <= maxx)
	Left  x'' -> (minx >= x'') || (x'' >= maxx)
 	where
		x' = rangeCheck minx maxx x
		types = x :: Int
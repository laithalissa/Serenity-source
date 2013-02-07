
module Log(logprint) where

import System.IO.Unsafe(unsafePerformIO)

logprint :: (Show m) => m -> a -> a
logprint msg result = unsafePerformIO $ do
	print msg
	return result

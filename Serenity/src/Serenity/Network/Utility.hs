module Serenity.Network.Utility
(	readNTChan
,	readTChanUntilEmpty
,	sendMessages
) where

import Control.Concurrent.STM
import Control.Monad (liftM, mapM_)

import Serenity.Model.Message (Message)

-- | Read the first n items from a TChan
readNTChan :: Int -> TChan a -> IO [a]
readNTChan n tchan = liftM (take n) (readTChanUntilEmpty tchan)

-- | Read all of the items from a TChan
readTChanUntilEmpty :: TChan a -> IO [a]
readTChanUntilEmpty chan = readTChanUntilEmpty' chan []

readTChanUntilEmpty' :: TChan a -> [a] -> IO [a]
readTChanUntilEmpty' tchan accum = do
	value <- atomically $ tryReadTChan tchan
	case value of
		Nothing -> return accum
		Just v -> readTChanUntilEmpty' tchan (accum ++ [v])

sendMessages :: TChan Message -> [Message] -> IO ()
sendMessages chan messages = atomically $ mapM_ (writeTChan chan) messages

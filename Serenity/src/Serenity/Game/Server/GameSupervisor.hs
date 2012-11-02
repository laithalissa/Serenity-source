
module Serenity.Game.Server.GameSupervisor where

import Control.Monad.STM(STM, atomically)
import Control.Concurrent.STM.TVar(TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent(forkIO, threadDelay)


type Buffer = STM (TVar [Int])


start :: IO ()
start = do
  putStrLn "server started"
  makeThreads makeBuffer
  threadDelay 5000000
  putStrLn "server finished"

makeBuffer :: Buffer
makeBuffer = newTVar []

makeThreads buffer = do
  producerThreadId <- forkIO (producer buffer 1)
  threadDelay 1000
  consumerThreadId <- forkIO (consumer buffer)
  return ()


while :: (IO Bool) -> IO ()
while predicate = do
  result <- predicate
  if result then return () else while predicate


consumer buffer = while $ do
                    result <- dequeue buffer
                    case result of 
                      Nothing -> do
                                  print "nothing to consume, finishing"
                                  return True
                      (Just val) -> do
                                  print ("consumed " ++ (show val))
                                  return False

-- consumer buffer = do 
--   result <- dequeue buffer
--   case result of
--     Nothing -> do
--                 print "nothing to consume"
--                 consumer buffer
--     (Just val) -> do
--                 print "consumed " ++ (show val)
--                 consumer buffer
--     _ -> return 


dequeue :: Buffer -> IO (Maybe Int)
dequeue buffer = atomically $ do
  bufferTVar <- buffer
  bufferVal <- readTVar bufferTVar
  if (null bufferVal)
     then return Nothing
     else do
       writeTVar bufferTVar (tail bufferVal)
       return (Just (head bufferVal))

producer :: Buffer -> Int -> IO ()
producer buffer nextCount = 
    while $ do
      print ("producer push next value: " ++ (show nextCount))
      push buffer nextCount
      return False

push :: Buffer -> Int -> IO ()
push buffer value = atomically $ do
  bufferTVar <- buffer
  bufferVal <- readTVar bufferTVar
  writeTVar bufferTVar (bufferVal ++ [value])
  return ()

                              
poll :: Buffer -> IO Int







data Command = Move | Attack | Quit deriving(Show)
  


















    









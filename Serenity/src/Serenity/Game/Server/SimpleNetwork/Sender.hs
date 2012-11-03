
module Serenity.Game.Server.SimpleNetwork.Sender 
( connect
, Sender(..)
) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Serenity.Network.Transport as T

connect :: String -> Int -> IO BasicSender
connect address port = do
  connection <- T.run_connect address (fromIntegral port)
  connectionRef <- newTVarIO connection
  return (BasicSender connectionRef)

class Sender a where
  send :: a -> String -> IO ()
  

data BasicSender = BasicSender {
  pointer :: TVar T.Connection
}

instance Sender BasicSender where
  -- send basicSender message = do
  --   atomically $ do
  --     beforeConnection <- readTVar (pointer basicSender)
  --     (_, afterConnection) <- T.run_transport (T.send message) beforeConnection
  --     writeTVar (pointer basicSender) afterConnection
      
  send basicSender message = do
    beforeConnection <- readTVarIO (pointer basicSender)
    (_, afterConnection) <- T.run_transport (T.send message) beforeConnection
    atomically $ writeTVar (pointer basicSender) afterConnection
    
  


module Serenity.Game.Server.SimpleNetwork.Receiver
( connect
, Receiver(..)
) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Serenity.Network.Transport as T

connect :: Int -> IO BasicReceiver
connect port = do
  connection <- T.run_listen (fromIntegral port)
  connectionRef <- newTVarIO connection
  return (BasicReceiver connectionRef)

class Receiver a where
  receive :: a -> IO String
  
data BasicReceiver = BasicReceiver {
  pointer :: TVar T.Connection
}

instance Receiver BasicReceiver where
  receive basicReceiver = do
    connection <- readTVarIO (pointer basicReceiver)
    T.eval_transport(T.receive) connection
    





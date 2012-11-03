
module Serenity.Game.Server.SimpleNetwork.Receiver
( connect
, Receiver(..)
) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import System.Timeout(timeout)

import qualified Serenity.Network.Transport as T

connect :: Int -> IO BasicReceiver
connect port = do
  connection <- T.run_listen (fromIntegral port)
  connectionRef <- newTVarIO connection
  return (BasicReceiver connectionRef)

class Receiver a where
  -- |receives a messages from a client. blocks until a message is received.
  receive :: a -> IO String
  
  -- |receives a message from a client. takes an additional parameter of timeout, in milliseconds. Will return with either the message has been received or the timeout has expired.
  receiveTimeout :: a -> Int -> IO (Maybe String)
  
data BasicReceiver = BasicReceiver {
  pointer :: TVar T.Connection
}

instance Receiver BasicReceiver where
  receive basicReceiver = do
    connection <- readTVarIO (pointer basicReceiver)
    T.eval_transport(T.receive) connection
    
  receiveTimeout basicReceiver waitTime = timeout (waitTime*1000) (receive basicReceiver)
    




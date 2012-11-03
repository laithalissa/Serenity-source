
module Serenity.Game.Server.Network 
( newClientSocket
, newServerSocket
, Socket(..)
, NetworkSocket
) where


import Control.Concurrent.STM.TVar
import Control.Monad.STM
import System.Timeout(timeout)

import qualified Serenity.Network.Transport as T



newClientSocket :: String -> Int -> IO NetworkSocket
newClientSocket address port = do
  connection <- T.run_connect address (fromIntegral port)
  connectionRef <- newTVarIO connection
  return (NetworkSocket connectionRef)
  
newServerSocket :: Int -> IO NetworkSocket
newServerSocket port = do
  connection <- T.run_listen (fromIntegral port)
  connectionRef <- newTVarIO connection
  return (NetworkSocket connectionRef)




class Socket a where
  send :: a -> String -> IO ()
  
  -- |receives a messages from a client. blocks until a message is received.
  listen :: a -> IO String
  
  -- |receives a message from a client. takes an additional parameter of timeout, in milliseconds. Will return with either the message has been received or the timeout has expired.
  listenTimeout :: a -> Int -> IO (Maybe String)

<<<<<<< HEAD
  getAddress :: a -> IO String

=======
>>>>>>> updated ant build so it will run the tests with the environment folder with its resources, ant build script refactored, much cleaner, refactored the Receiver/Sender modules into a single abstract Socket concept within the Server/Network module, added a test for the Server/Network module

data NetworkSocket = NetworkSocket { pointer :: TVar T.Connection }

instance Socket NetworkSocket where
  
<<<<<<< HEAD
  getAddress socket = do
    connection <- readTVarIO (pointer socket)
    (return . show . T.connection_addr) connection
  
=======
>>>>>>> updated ant build so it will run the tests with the environment folder with its resources, ant build script refactored, much cleaner, refactored the Receiver/Sender modules into a single abstract Socket concept within the Server/Network module, added a test for the Server/Network module
  send socket message = do
    beforeConnection <- readTVarIO (pointer socket)
    (_, afterConnection) <- T.run_transport (T.send message) beforeConnection
    atomically $ writeTVar (pointer socket) afterConnection

<<<<<<< HEAD
  -- listen socket = do
  --   beforeConnection <- readTVarIO (pointer socket)
  --   (msg, afterConnection) <- T.eval_transport (do T.receive; T.get_connection) beforeConnection
  --   atomically $ writeTVar (pointer socket) afterConnection
  --   return msg
    
=======
>>>>>>> updated ant build so it will run the tests with the environment folder with its resources, ant build script refactored, much cleaner, refactored the Receiver/Sender modules into a single abstract Socket concept within the Server/Network module, added a test for the Server/Network module
  listen socket = do
    connection <- readTVarIO (pointer socket)
    T.eval_transport(T.receive) connection
    
  listenTimeout socket waitTime = timeout (waitTime*1000) (listen socket)

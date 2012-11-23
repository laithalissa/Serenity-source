module Serenity.Game.Server.Network where
-- ( newClientSocket
-- , newServerSocket
-- , Socket(..)
-- , NetworkSocket
-- ) where


import Control.Concurrent.STM.TVar
import Control.Monad.STM
import System.Timeout(timeout)

import qualified Serenity.Network.Transport as T



-- newClientSocket :: String -> Int -> IO NetworkSocket
-- newClientSocket address port = do
--   connection <- T.run_connect address (fromIntegral port)
--   connectionRef <- newTVarIO connection
--   return (NetworkSocket connectionRef)
  
-- newServerSocket :: Int -> IO NetworkSocket
-- newServerSocket port = do
--   connection <- T.run_listen (fromIntegral port)
--   connectionRef <- newTVarIO connection
--   return (NetworkSocket connectionRef)




-- class Socket a where
--   send :: a -> String -> IO ()
  
--   -- |receives a messages from a client. blocks until a message is received.
--   listen :: a -> IO String
  
--   -- |receives a message from a client. takes an additional parameter of timeout, in milliseconds. Will return with either the message has been received or the timeout has expired.
--   listenTimeout :: a -> Int -> IO (Maybe String)

--   getAddress :: a -> IO String


-- data NetworkSocket = NetworkSocket { pointer :: TVar T.Connection }

-- instance Socket NetworkSocket where
  
--   getAddress socket = do
--     connection <- readTVarIO (pointer socket)
--     (return . show . T.connection_addr) connection
  
--   send socket message = do
--     beforeConnection <- readTVarIO (pointer socket)
--     (_, afterConnection) <- T.run_transport (T.send message) beforeConnection
--     atomically $ writeTVar (pointer socket) afterConnection

--   listen socket = do
--     connection <- readTVarIO (pointer socket)
--     T.eval_transport(T.receive) connection
    
--   listenTimeout socket waitTime = timeout (waitTime*1000) (listen socket)

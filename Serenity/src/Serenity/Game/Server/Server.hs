
module Serenity.Game.Server.Server where

import Serenity.Game.Server.Network
import Serenity.Game.Server.GameRunner as GR

masterClientPort = 9000
slaveClientPort = 9050

start :: IO ()
start = GR.main

start2 :: IO ()
start2 = do
  socket <- newServerSocket 9900
  msg1 <- listen socket
  socketAddress <- getAddress socket
  print $ "client connected with address " ++ socketAddress
  print $ "server received message: " ++ msg1
  msg2 <- listenTimeout socket 3000
  print $ "server received message: " ++ (show msg2)
  
  
-- waitForMasterClient :: (Socket a) => IO a
-- waitForMasterClient = do
--   socket <- newServerSocket masterClientPort
--   loop socket
--   where
--     loop socket = do
--       msg <- listen socket 
--       print "client connected"
--       if msg == "MASTER" 
--          then do
--            print "master client connected" 
--            return msg
--          else do
--            print "client isn't master, dropping"
--            loop socket
  

  

  
  


-- start = do
--   print $ "server started on port " ++ (show port)
--   connection <- run_listen port
--   msg <- eval_transport (receive) conNection
--   print $ "server received: " ++ msg
--   msg2 <- eval_transport (receive) connection
--   print $ "server received: " ++ msg2
  
  

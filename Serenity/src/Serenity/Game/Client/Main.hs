module Serenity.Game.Client.Main (
	main
) 
where


<<<<<<< HEAD
import Serenity.Game.Server.Network

main = do
	connection <- runConnect "localhost" 9900
	return ()
=======
import Serenity.Game.Server.SimpleNetwork.Sender

main = do
  sender <- connect "localhost" 9900
  send sender "hello world"
  send sender "hehehehe"

-- import Serenity.Network.Transport
-- main = do
-- 	connection <- run_connect "localhost" 9900
--         (_, c2) <- run_transport (send "hello, i am the master client") connection
--         run_transport (send "this is second message") c2
-- 	return ()

>>>>>>> added an impure network layer for server to use, updated server and client to use this layer

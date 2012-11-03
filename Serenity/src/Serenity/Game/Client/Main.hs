module Serenity.Game.Client.Main (
	main
) 
where


import Serenity.Game.Server.SimpleNetwork.Sender

main = do
	connection <- runConnect "localhost" 9900
	return ()


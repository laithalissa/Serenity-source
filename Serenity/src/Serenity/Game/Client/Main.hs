module Serenity.Game.Client.Main (
	main
) 
where


import Serenity.Game.Server.Network

main = do
	connection <- runConnect "localhost" 9900
	return ()



module Serenity.Game.Client.Main (
	main
) 
where

import Serenity.Network.Transport

main = do
	connection <- run_connect "localhost" 9900
	return ()
module Serenity.Game.Client.Main (
	main
) 
where

import Serenity.Network.Transport

main = do
	connection <- run_connect 9900
	return ()
module Serenity.Game.Client.Main (
	main
) 
where

import Serenity.Network.Transport

main = do
	connection <- runConnect "localhost" 9900
	return ()
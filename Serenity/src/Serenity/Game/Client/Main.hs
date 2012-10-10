module Serenity.Game.Client.Main (
	main
) 
where

import Serenity.Network

main = do
	connection <- connect 9900
	return ()
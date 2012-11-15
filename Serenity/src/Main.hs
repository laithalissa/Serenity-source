module Main 
(	main
) 
where

import qualified Serenity.Game.Server.Main as Server
import qualified Serenity.Game.Client.Main as Client

import Control.Monad (liftM)
import System.Environment (getArgs)

main = do
	arg <- liftM (\args -> if length args > 0 then args!!0 else "") getArgs
	case arg of
		"server" -> Server.main
		"client" -> Client.main
		"" -> putStrLn "first argument must either be 'server' or 'client'"
		_ -> putStrLn ("unknown arg '" ++ arg ++ "', first argument must either be 'server' or 'client'")

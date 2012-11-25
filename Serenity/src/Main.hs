module Main
(	main
)
where

import qualified Serenity.Game.Server.Main as Server
import qualified Serenity.Game.Client.Main as Client

import Control.Monad (liftM)
import System.Environment (getArgs)

main = do
	args <- getArgs
	let command = if length args > 0 then head args else ""

	case command of
		"server" -> Server.main
		"client" -> Client.main (tail args)
		"" -> putStrLn "first argument must either be 'server' or 'client'"
		_ -> putStrLn ("unknown arg '" ++ command ++ "', first argument must either be 'server' or 'client'")

module Main
(	main
)
where

import Serenity.Game.Server.Main(server)
import Serenity.Game.Client.Main(client)

import Control.Monad (liftM)
import System.Environment (getArgs)

import System.Console.ParseArgs
	(	Arg(..)
	,	Argtype(..)
	,	ArgsComplete(ArgsComplete)
	,	parseArgsIO
	,	argsDataRequired
	,	argsDataOptional
	,	argsDataDefaulted
	)

main :: IO ()
main = do
	

-- main = do
-- 	args <- getArgs
-- 	let command = if length args > 0 then head args else ""

-- 	case command of
-- 		"server" -> Server.main
-- 		"client" -> Client.main (tail args)
-- 		"" -> putStrLn "first argument must either be 'server' or 'client'"
-- 		_ -> putStrLn ("unknown arg '" ++ command ++ "', first argument must either be 'server' or 'client'")

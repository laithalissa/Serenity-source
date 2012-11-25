module Main
(	main
)
where

import Serenity.Game.Server.Main(server)
import Serenity.Game.Client.Main(client)

import Control.Monad (liftM)
import System.Environment (getArgs)
import Data.Maybe(fromJust)

import System.Console.ParseArgs
	(	Args
	,	Arg(..)
	,	Argtype(..)
	,	ArgsComplete(ArgsComplete, ArgsInterspersed)
	,	parseArgsIO
	,	argDataRequired
	,	argDataOptional
	,	argDataDefaulted
	,	usageError
	,	getArgString
	,	getArgInt
	,	gotArg
	)


clientMain :: IO (Args String)
clientMain = parseArgsIO 
		ArgsComplete
		[	Arg
			{	argIndex="mode"
			,	argAbbr=Just 'm'
			,	argName=Just "mode"
			,	argData=argDataRequired "client/server" ArgtypeString
			,	argDesc="wheather to start in server or client mode"
			}
		,	Arg
			{	argIndex="port"
			,	argAbbr=Just 'p'
			,	argName=Just "port"
			,	argData=argDataDefaulted "port" ArgtypeInt 9050
			,	argDesc="port to connect/listen to"
			}
		,	Arg
			{	argIndex="host"
			,	argAbbr=Just 'h'
			,	argName=Just "host"
			,	argData=argDataRequired "host" ArgtypeString
			,	argDesc="host the server is running at, only applicable with --mode client"
			}
		,	Arg
			{	argIndex="playerName"
			,	argAbbr=Just 'n'
			,	argName=Just "player-name"
			,	argData=argDataRequired "player-name" ArgtypeString
			,	argDesc="name of player"
			}
		
		]

serverMain :: IO (Args String)
serverMain = parseArgsIO 
		ArgsComplete
		[	Arg
			{	argIndex="mode"
			,	argAbbr=Just 'm'
			,	argName=Just "mode"
			,	argData=argDataRequired "client/server" ArgtypeString
			,	argDesc="wheather to start in server or client mode"
			}
		,	Arg
			{	argIndex="port"
			,	argAbbr=Just 'p'
			,	argName=Just "port"
			,	argData=argDataDefaulted "port" ArgtypeInt 9050
			,	argDesc="port to connect/listen to"
			}
		,	Arg
			{	argIndex="clientCount"
			,	argAbbr=Just 'c'
			,	argName=Just "client-count"
			,	argData=argDataDefaulted "number" ArgtypeInt 1
			,	argDesc="number of client to wait for before starting server"
			}
		
		]

topMain :: IO (Args String)
topMain = parseArgsIO 
		ArgsInterspersed
		[	Arg
			{	argIndex="mode"
			,	argAbbr=Just 'm'
			,	argName=Just "mode"
			,	argData=argDataRequired "client/server" ArgtypeString
			,	argDesc="wheather to start in server or client mode"
			}		
		]

main :: IO ()
main = do
	args <- topMain
	
	case (fromJust $ getArgString args "mode") of
		"server" -> do
			sArgs <- serverMain
			server 
				(fromJust $ getArgInt sArgs "port")
				(fromJust $ getArgInt sArgs "clientCount")
				
		"client" -> do
			sArgs <- clientMain
			client
				(fromJust $ getArgString sArgs "host")
				(fromJust $ getArgInt sArgs "port")
				(fromJust $ getArgString sArgs "playerName")
				
		_ -> print "invalid mode, must be either 'server' or 'client'"

	
-- 	case specificArgs of
-- 		Nothing -> 
-- 		Just sArgs -> c
	



-- 	if (fst $ validateArgs args)
-- 		then handleArgs args
-- 		else print (snd $ validateArgs args)

	
-- validateArgs :: Args String -> (Bool, String)
-- validArgs args = if (length topValidate) == 0 
-- 			then (True, "")
-- 			else head topValidate

-- 	where

-- 	topValidate = if (fst modeValid)
-- 			then case mode of
-- 				"server" -> validServer
-- 				"client" -> validClient
-- 			else [modeValid]
			

-- 	validServer = 
-- 		[	portValid
-- 		,	if gotArg args "host" 
-- 				then (False, "server mode should have host")
-- 				else (True, "")
-- 		]

-- 	validClient = 
-- 		[	portValid
-- 		,	if gotArg args "host"
-- 				then hostValid
-- 				else (False, "host is required for client mode")
-- 		]

-- 	modeValid = if mode `elem` ["client", "server"]
-- 				then (True, "")
-- 				else (False, "invalid mode, must either be 'client' or 'server'")
-- 	portValid = if (port >  0 && port <= 10000) 
-- 				then (True, "")
-- 				else (False, "port is out of range")
-- 	hostValid = if " " `elem` host
-- 				then (False "host can't contain spaces")
-- 				else (True, "")

-- 	mode = (fromJust $ getArgstring args "mode")
-- 	port = (fromJust $ getArgint args "port")
-- 	host = (fromJust $ getArgstring args "host")
	
	

-- handleArgs :: Args String -> IO ()
-- handleArgs args = case (fromJust $ getArgString args "mode") of
-- 	"server" -> server (getArg
-- 	"client" -> client

	

-- main = do
-- 	args <- getArgs
-- 	let command = if length args > 0 then head args else ""

-- 	case command of
-- 		"server" -> Server.main
-- 		"client" -> Client.main (tail args)
-- 		"" -> putStrLn "first argument must either be 'server' or 'client'"
-- 		_ -> putStrLn ("unknown arg '" ++ command ++ "', first argument must either be 'server' or 'client'")

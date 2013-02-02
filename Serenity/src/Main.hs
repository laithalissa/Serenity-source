module Main
(	main
)
where

import Serenity.Game.Server.Main (server)
import Serenity.Game.Client.Main (client)

import System.Console.ParseArgs
	(	Args
	,	Arg(..)
	,	Argtype(..)
	,	ArgsComplete(..)
	,	parseArgsIO
	,	argDataRequired
	,	argDataDefaulted
	,	getRequiredArg
	)

clientMain :: IO (Args String)
clientMain = parseArgsIO 
		ArgsInterspersed
		[	Arg
			{	argIndex="host"
			,	argAbbr=Just 'h'
			,	argName=Just "host"
			,	argData=argDataRequired "host" ArgtypeString
			,	argDesc="Address of the server to connect to"
			}
		,	Arg
			{	argIndex="port"
			,	argAbbr=Just 'p'
			,	argName=Just "port"
			,	argData=argDataDefaulted "port" ArgtypeInt 9050
			,	argDesc="Port on the server to connect to (defaults to 9050)"
			}
		,	Arg
			{	argIndex="playerName"
			,	argAbbr=Just 'n'
			,	argName=Just "player-name"
			,	argData=argDataRequired "player-name" ArgtypeInt
			,	argDesc="Name of player"
			}
		]

serverMain :: IO (Args String)
serverMain = parseArgsIO 
		ArgsInterspersed
		[	Arg
			{	argIndex="port"
			,	argAbbr=Just 'p'
			,	argName=Just "port"
			,	argData=argDataDefaulted "port" ArgtypeInt 9050
			,	argDesc="Port to listen for clients on (defaults to 9050)"
			}
		,	Arg
			{	argIndex="clientCount"
			,	argAbbr=Just 'c'
			,	argName=Just "client-count"
			,	argData=argDataDefaulted "number" ArgtypeInt 1
			,	argDesc="Number of clients required to start a game (defaults to 1)"
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
			,	argDesc="Whether to start in server or client mode"
			}		
		]

main :: IO ()
main = do
	args <- topMain
	case getRequiredArg args "mode" of
		"server" -> do
			sArgs <- serverMain
			server 
				(getRequiredArg sArgs "port")
				(getRequiredArg sArgs "clientCount")
				
		"client" -> do
			cArgs <- clientMain
			client
				(getRequiredArg cArgs "host")
				(getRequiredArg cArgs "port")
				(getRequiredArg cArgs "playerName")

		_ -> print "invalid mode, must be either 'server' or 'client'"

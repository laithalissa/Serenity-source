module Main
(	main
)
where

import Serenity.Game.Client.Main (client)
import Serenity.Game.Server.Main (server)
import Serenity.Game.UI.Main     (gui)
import Serenity.Model
import System.Console.ParseArgs

clientMainArgs :: IO (Args String)
clientMainArgs = parseArgsIO 
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

serverMainArgs :: IO (Args String)
serverMainArgs = parseArgsIO 
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

topMainArgs :: IO (Args String)
topMainArgs = parseArgsIO 
		ArgsInterspersed
		[	Arg
			{	argIndex="mode"
			,	argAbbr=Just 'm'
			,	argName=Just "mode"
			,	argData=argDataDefaulted "client/server/gui" ArgtypeString "gui"
			,	argDesc="Whether to start in server or client mode"
			}		
		]

main :: IO ()
-- main = do
-- 	assets <- initAssets "/Users/joseph/Projects/WorkingArea/Serenity/Serenity-source/Serenity/resources/templates"
-- 	print assets
-- 	return ()


main = do
	args <- topMainArgs
	case getRequiredArg args "mode" of
		"server" -> do
			sArgs <- serverMainArgs
			server 
				sectorOne
				(getRequiredArg sArgs "port")
				(getRequiredArg sArgs "clientCount")
				
		"client" -> do
			cArgs <- clientMainArgs
			client
				(getRequiredArg cArgs "host")
				(getRequiredArg cArgs "port")
				(getRequiredArg cArgs "playerName")

		"gui" -> gui

		_ -> print $ argsUsage args

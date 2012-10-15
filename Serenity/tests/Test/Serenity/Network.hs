module Test.Serenity.Network (test_group) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Test.HUnit

import Serenity.Network

test_group = testGroup "Network Tests" 
	[	testCase "Test a listening process accepts a connection" test_acceptance
	]

client :: IO ()
client = do
	connection <- connect 9900
	return ()

server :: TVar (Maybe Connection) -> IO ()
server tvar = do
	connection <- listen 9900
	atomically $ writeTVar tvar $ Just connection
	return ()

is_connected :: Connection -> Bool
is_connected Connected {} = True
is_connected _ = False

is_connected_maybe :: Maybe Connection -> Bool
is_connected_maybe (Just con) = is_connected con
is_connected_maybe _ = False

test_acceptance = do
	serverConnection <- atomically $ newTVar (Nothing :: Maybe Connection)
	forkIO $ server serverConnection
	forkIO client
	serverConn <- atomically $ do 
		maybe_connection <- readTVar serverConnection
		case maybe_connection of
			Nothing -> retry
			Just connection -> return connection
	True @=? (is_connected serverConn)


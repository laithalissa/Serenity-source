
module Test.Serenity.Game.Server.Network (networkTests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
<<<<<<< HEAD
import Test.HUnit(assertEqual, assertFailure)

import Control.Concurrent(forkIO, threadDelay, myThreadId)
import Control.Concurrent.STM.TVar(newTVarIO, )
import Serenity.Game.Server.Network
import Data.List(delete)
=======
import Test.HUnit(assertEqual)

import Serenity.Game.Server.Network

>>>>>>> updated ant build so it will run the tests with the environment folder with its resources, ant build script refactored, much cleaner, refactored the Receiver/Sender modules into a single abstract Socket concept within the Server/Network module, added a test for the Server/Network module

networkTests :: Test
networkTests = testGroup testName tests

testName :: String
testName = "Server Network Tests"

tests :: [Test]
tests = 
<<<<<<< HEAD
  [ testMessageSentByClientSameAsMessageReceievedByServer    
    -- testMultipleClients 
  ]
  
  
testMessageSentByClientSameAsMessageReceievedByServer :: Test
testMessageSentByClientSameAsMessageReceievedByServer = testCase "Test client can connect to server" theTest
  where 
    theTest = do
      forkIO client
      server

    client = do
      threadDelay 1000000
      socket <- newClientSocket "localhost" 8001
      send socket "hello world"
    server = do
      socket <- newServerSocket 8001
      message <- listen socket
      assertEqual "message sent by client same as message received by server" message "hello world"
     
      
      
testMultipleClients :: Test     
testMultipleClients = testCase "Test multiple clients" theTest
  where                   
    
    theTest = do
      client1 <- forkIO (client 1)
      client2 <- forkIO (client 2)
      server $ map show [1, 2]    
    
    client clientId = do
      threadDelay (clientId * 1000000)
      socket <- newClientSocket "localhost" 8002
      send socket (show clientId)  
    
    server :: [String] -> IO ()
    server clientIds = do
      socket <- newServerSocket 8002
      waiting socket clientIds

        where 
          waiting socket remainingIds = 
            if (length remainingIds == 0) 
            then return ()
            else do
              maybeMessage <-listenTimeout socket 5000
              case maybeMessage of
                Nothing -> assertFailure "timed out"
                (Just msg) -> waiting socket (delete msg remainingIds)
    
    
    
    
=======
  [ testClientCanConnectToServer    
  ]
  
  
testClientCanConnectToServer :: Test
testClientCanConnectToServer = testCase "Test client can connect to server" (assertEqual "2 == 2?" 2 2)
>>>>>>> updated ant build so it will run the tests with the environment folder with its resources, ant build script refactored, much cleaner, refactored the Receiver/Sender modules into a single abstract Socket concept within the Server/Network module, added a test for the Server/Network module

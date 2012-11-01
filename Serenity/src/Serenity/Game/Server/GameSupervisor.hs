
module Serenity.Game.Server.GameSupervisor where

type Scenario = String
type Client = String

start :: Scenario -> [Client] -> IO ()
start scenario clients = do
    putStrLn ("running scenario: " ++ scenario)
    (putStrLn . (++) "number of clients: " . show . length) clients
    









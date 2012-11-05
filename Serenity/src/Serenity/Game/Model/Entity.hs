
module Serenity.Game.Model.Entity where

data Entity = Entity { 
              entityName :: String,
              entityOwner :: String,
              entityLocation :: (Int, Int)
              } deriving(Show, Ord, Eq)

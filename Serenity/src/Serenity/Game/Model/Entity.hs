
module Serenity.Game.Model.Entity where

import Serenity.Game.Model.Common

data Entity = 
     Ship { 
          shipId :: Int,
          location :: Location,
          direction :: Direction,
          speed :: Float
          } |
     Gun { 
         shipId :: Int,
         weaponSlotIndex :: Int
          } |
     System {
            shipId :: Int
            }  |
     Bullet {
            location :: Location,
            direction :: Direction,
            speed :: Float
            }
            |
     Planet {
            planetName :: String,
            owner :: Maybe String
            } deriving (Show, Ord, Eq)



module Serenity.Game.Model.ShipOrder where

import Serenity.Game.Model.Common

data ShipOrder = 
    NoOrder |   
    MoveShipOrder 
    {         position :: Location
    ,         direction :: Direction 
    } deriving(Show, Eq, Ord)
    
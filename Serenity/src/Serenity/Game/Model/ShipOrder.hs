
module Serenity.Game.Model.ShipOrder where

import Serenity.Game.Model.Common

data ShipOrder = 
    MoveToPosition { position :: Location, direction :: Direction } |
    
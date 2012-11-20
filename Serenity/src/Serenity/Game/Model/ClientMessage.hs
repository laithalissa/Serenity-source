
module Serenity.Game.Model.ClientMessage where

import Serenity.Game.Model.Entity
import Serenity.Game.Model.Common

data ClientMessage = 
  ClientScroll ViewPort | 
  ClientMoveOrder 
  {               clientMoveOrderShipId :: EntityId 
  ,               clientMoveOrderLocation :: Location
  } |
  ClientStillOrder { clientStillShipId :: EntityId }
  deriving(Show, Eq)


data WorldDelta = 
     EntityAdded String |
     EntityDeleted Int deriving(Show)

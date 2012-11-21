
module Serenity.Game.Model.ClientMessage where

import Serenity.Game.Model.Entity
import Serenity.Game.Model.Common

data ClientMessage =
	ClientMessageGraphics GraphicsMessage |
	ClientMessageWorld WorldMessage
	deriving(Show, Eq)                                               


data GraphicsMessage = ClientScroll ViewPort deriving(Show, Eq)

data WorldMessage = 	
	ClientMoveOrder 
	{	clientMoveOrderShipId :: EntityId 
	,	clientMoveOrderLocation :: Location
	} |
	ClientStillOrder 
	{	clientStillShipId :: EntityId 
	}
	deriving(Show, Eq)

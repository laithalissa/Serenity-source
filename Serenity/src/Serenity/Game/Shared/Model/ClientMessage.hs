module Serenity.Game.Shared.Model.ClientMessage where

import Serenity.Game.Shared.Model.Entity
import Serenity.Game.Shared.Model.Common

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

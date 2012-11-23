{-# LANGUAGE TemplateHaskell #-}

module Serenity.Network.Message 
(	Message (..)
)
where

import Data.Word (Word32, Word16)
import Data.Binary
import Data.DeriveTH

data Message = 
	Empty
	| UpdateLocation
	{	entityId :: Word32
	,	location :: (Word16, Word16)
	}
	| KillShip
	{	entityId :: Word32
	}
	deriving (Show, Eq)

$(derive makeBinary ''Message)
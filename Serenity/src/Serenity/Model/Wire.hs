module Serenity.Model.Wire 
(	module Control.Wire
,	BaseWire
,	UpdateWire
,	runWire
,	
)where

import Prelude hiding (id, (.))

import Control.Wire
import Control.Monad.Identity (Identity, runIdentity)
import GHC.Float

import Serenity.Model.Message

type BaseWire = Wire LastException Identity
type UpdateWire a = BaseWire a [Update]

runWire :: (UpdateWire a) -> Float -> a -> ([Update], UpdateWire a)
runWire wire timeDelta a = (updates result, newWire) where 
	(result, newWire) = runIdentity $ stepWire wire (float2Double timeDelta) a
	updates (Right u) = u
	updates (Left _ ) = []
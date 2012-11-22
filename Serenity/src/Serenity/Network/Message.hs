module Serenity.Network.Message 
(	Message (..)
)
where

import Data.Word (Word32, Word16)
import Data.Binary (Binary(..))
import Data.Binary.Put
import Data.Binary.Get
import Data.Monoid (mempty)

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

instance Binary Message where
	put message = do
		putWord16be messageId
		putMessage message where
			(messageId, putMessage) = putMessageAndID message
	get = do
		messageId <- getWord16be
		getMessage messageId

putMessageAndID message = case message of
	Empty             -> (0, \_ -> return ())
	UpdateLocation {} -> (1, putUpdateLocation)
	KillShip {}       -> (2, putKillShip)

getMessage 0 = return Empty
getMessage 1 = getUpdateLocation
getMessage 2 = getKillShip

putUpdateLocation UpdateLocation {entityId = eID, location = (x,y)} = do
	putWord32be eID
	putWord16be x
	putWord16be y
getUpdateLocation = do
	eID <- getWord32be
	x <- getWord16be
	y <- getWord16be
	return UpdateLocation {entityId = eID, location = (x,y)}

putKillShip KillShip {entityId = eID} = do
	putWord32be eID
getKillShip = do
	eID <- getWord32be
	return KillShip {entityId = eID}
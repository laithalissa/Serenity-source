{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Serenity.Network.Transport
(	Connection (..)
,	Packet (..)
,	runTransport
,	evalTransport
,	runConnect
,	runListen
,	connect
,	listen
,	send
,	receive
,	isConnected
,	getConnection
,	liftIO
,	Transport
,	MonadTransport
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom, SocketStatus(..), accept, listen, connect, isConnected)
import Network.Socket.ByteString hiding (send)
import System.Posix.IO
import Data.Time.Clock.POSIX

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (liftM)
import Control.Monad.State
import Data.Monoid

import Serenity.Network.Packet

import Data.Binary (Binary)

import Serenity.Network.Message (Message)
import qualified Serenity.Network.Message as Message

data Connection =
	Connected
	{	connectionSocket :: Socket
	,	connectionAddr :: SockAddr
	,	connectionId :: Int
	,	connectionSent :: Set (Packet, POSIXTime)
	,	connectionLocalSequence ::  Int
	,	connectionRemoteSequence ::  Int
	}
	| Unconnected deriving (Show, Eq)

initialConnection sock addr cid = Connected
	{	connectionSocket = sock
	,	connectionAddr = addr
	,	connectionId = cid
	,	connectionSent = Set.empty
	,	connectionLocalSequence = 1
	,	connectionRemoteSequence = 1
	}

isConnected :: Connection -> Bool
isConnected Connected {} = True
isConnected _ = False

bindOutboundSocket host port = withSocketsDo $ do
	addrInfo <-
		liftM head $
		liftM (filter (\x -> addrFamily x == AF_INET)) $
		getAddrInfo Nothing (Just "localhost") (Just (show port))
	let addr = addrAddress addrInfo
	let family = addrFamily addrInfo

	sock <- socket family Datagram 0
	bindSocket sock (SockAddrInet (port+1) iNADDR_ANY)
	return (sock, addr, family)

class MonadIO m => MonadTransport m where
	connect :: String -> PortNumber -> m ()
	listen  :: PortNumber -> m ()
	send :: Message -> m ()
	receive :: m Message
	getConnection :: m Connection

newtype Transport a = Transport (StateT Connection IO a)
	deriving (Functor, Monad, MonadIO, MonadState Connection)

instance MonadTransport Transport where
	connect host port = do
		connection <- get
		case connection of
			Connected {} -> return ()
			Unconnected -> do
				(sock, addr, family) <- liftIO $ bindOutboundSocket host port
				put $ initialConnection sock addr 13
				liftIO $ sendPacket sock emptySynPacket addr
				return ()

	listen port = do
		connection <- liftIO (withSocketsDo $ do
			sock <- socket AF_INET Datagram 0
			bindSocket sock (SockAddrInet port iNADDR_ANY)

			(packet, client) <- receivePacket sock
			if Syn `elem` (getFlags packet)
				then return $ initialConnection sock client 12
				else return Unconnected)
		put connection

	send string = do
		connection <- get
		liftIO $ sendPacket
			(connectionSocket connection)
			(initialPacket string)
			(connectionAddr connection)

	receive = do
		connection <- get
		(packet, client) <- liftIO $ receivePacket (connectionSocket connection)
		return $ getPacketData packet

	getConnection = get

instance (Monoid a) => Monoid (Transport a) where
	mempty = return mempty
	m1 `mappend` m2 = do
		x1 <- m1
		x2 <- m2
		return (x1 `mappend` x2)

evalTransport :: Transport a -> Connection -> IO a
evalTransport (Transport c) =  evalStateT c

runTransport :: Transport a -> Connection -> IO (a, Connection)
runTransport (Transport c) =  runStateT c

runOne :: Transport a -> IO Connection
runOne f = do
	(_, connection) <- runTransport f Unconnected
	return connection

runConnect :: String -> PortNumber -> IO Connection
runConnect host port = runOne $ connect host port

runListen :: PortNumber -> IO Connection
runListen port = runOne $ listen port


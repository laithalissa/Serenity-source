{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Quick where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Model
import Serenity.Game.Server.Main
import Serenity.Game.UI.Host hiding (aHost, aPort)

import Control.Lens
import Control.Monad.State
import Control.Concurrent

data QuickData a = QuickData
	{	_quickTitleLabel    :: Label a
	,	_quickVersionLabel  :: Label a
	}
makeLenses ''QuickData

class AppState a => QuickState a where
	aQuick :: Simple Lens a (QuickData a)
	aHost  :: Simple Lens a (HostData a)
	aHostName :: Simple Lens a String
	aPort :: Simple Lens a String

initQuickData :: QuickState a => Assets -> QuickData a
initQuickData assets = QuickData
	{	_quickTitleLabel    = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_quickVersionLabel  = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	}

viewQuick :: QuickState a => a -> View a
viewQuick a = (initView ((0, 0), (1024, 750))) 
	&	(viewDepict .~ background (a^.aAssets))
	<++
	[	label a (aQuick.quickTitleLabel) ((30,650),(220,30))
	,	label a (aQuick.quickVersionLabel) ((0,0),(100,15))
	]

timeQuick :: QuickState a => Float -> a -> a
timeQuick dt a = a

timeQuickIO :: QuickState a => Float -> StateT a IO ()
timeQuickIO dt = do
	serverThreadID <- liftIO.forkIO $ server sectorTwo Unwinnable 9900 1
	aHost.hostServerGame .= (Running serverThreadID)
	aHostName .= "localhost"
	aPort .= "9900"
	aMode .= Lobby
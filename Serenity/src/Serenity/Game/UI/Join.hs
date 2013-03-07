{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Join where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External

import Control.Lens
import Control.Monad.State

data JoinData a = JoinData
	{	_joinTitleLabel   :: Label a
	,	_joinVersionLabel :: Label a
	,	_joinPlayButton   :: Button a ApplicationMode
	,	_joinBackButton   :: Button a ApplicationMode
	,	_joinAddressBox   :: TextBoxLabel a
	,	_joinPortBox      :: TextBox a
	,	_joinNickNameBox  :: TextBoxLabel a
	,	_joinNickName     :: String
	,	_joinAddress      :: String
	}

makeLenses ''JoinData

initJoinData :: Simple Lens a (JoinData a) -> Simple Lens a String -> Assets -> JoinData a
initJoinData aJoin aPort assets = JoinData
	{	_joinTitleLabel   = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 6}
	,	_joinVersionLabel = (initLabel (StaticString serenityVersionString) (white) Nothing) {_labelScale = 1}
	,	_joinPlayButton   = initMenuButton "Play      ->" (\_ -> Lobby) & (buttonEnabled .~ playButtonEnabled aJoin aPort)
	,	_joinBackButton   = initMenuButton "<-      Back" (\_ -> Menu)
	,	_joinAddressBox   = (initMenuTextBoxLabel "Server:" (aJoin.joinAddress))
	,	_joinPortBox      = (initMenuTextBox aPort) & (tbPostEdit .~ portValidation)
	,	_joinNickNameBox  = (initMenuTextBoxLabel "Name:" (aJoin.joinNickName)) & (tblPostEdit .~ nameValidation)
	,	_joinNickName     = ""
	,	_joinAddress      = "localhost"
	}

playButtonEnabled :: Simple Lens a (JoinData a) -> Simple Lens a String -> a -> Bool
playButtonEnabled aJoin aPort a = all (/="") [a^.aJoin.joinNickName, a^.aJoin.joinAddress, a^.aPort]

viewJoin :: a -> Simple Lens a (JoinData a) -> Simple Lens a String -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewJoin a aJoin aPort aAssets aMode = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	label a (aJoin.joinTitleLabel) ((30,650),(220,30))
	,	label a (aJoin.joinVersionLabel) ((0,0),(100,15))
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	button a (aJoin.joinPlayButton) aMode ((80,650),(185,28))
		,	button a (aJoin.joinBackButton) aMode ((80, 50),(185,28))
		]
	,	(initBox ((20, 35), (650, 565))) <++ -- Main
		[	textBoxLabel a (aJoin.joinNickNameBox) (aJoin.joinNickName) ((14,520),(620,28)) 80
		,	textBoxLabel a (aJoin.joinAddressBox) (aJoin.joinAddress) ((14,470),(540,28)) 80
		,	textBox a (aJoin.joinPortBox) aPort ((555,470),(80,28))
		]
	]

timeJoin :: Simple Lens a (JoinData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeJoin aJoin aMode dt = id

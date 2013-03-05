module Serenity.Graphics.MainInterface where

import Serenity.Sheen
import Serenity.Model
import Serenity.Maths.Util
import Serenity.Game.Client.ClientState
import Serenity.Game.Client.GUI
import Serenity.Game.UI.Application

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import GHC.Float

import Data.Map (Map)
import qualified Data.Map as Map

instance ViewController ClientState where
	getView = clientStateView 
	globals = clientViewGlobals
	updateTime dt = id

clientStateView :: ClientState -> View ClientState
clientStateView clientState = (initView ((0,0),(1024, 750))) <++
	[	mainView clientState
	,	sidebarView clientState
	]

sidebarView clientState = (initBox ((0,0),(345,750)))
	& (viewBackground .~ Just translucentBackground)

mainView clientState = (initView ((0,0),(1024, 750)))
	& (viewDepict .~ undefined render)
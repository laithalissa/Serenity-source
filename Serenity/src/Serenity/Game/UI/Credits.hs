{-# LANGUAGE RankNTypes #-}

module Serenity.Game.UI.Credits where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.Game.Client.Color
import Serenity.External
import Serenity.Misc.GameNameGenerator

import Control.Lens
import Control.Monad.State

data CreditsData a = CreditsData
	{	_creditsTime           :: Float
	,	_creditsNumGamesShown  :: Int
	,	_creditsTitleLabel     :: Label a
	,	_creditsBackButton     :: Button a ApplicationMode
	,	_creditsALCLogoView    :: PictureView a
	,	_creditsMoreGamesLabel :: Label a
	,	_creditsAGameName      :: String
	,	_creditsAGameNameLabel :: Label a
	}
makeLenses ''CreditsData

class AppState a => CreditsState a where
	aCredits :: Simple Lens a (CreditsData a)

initCreditsData :: CreditsState a => Assets -> CreditsData a
initCreditsData assets = CreditsData
	{	_creditsTime           = 1000
	,	_creditsNumGamesShown  = 1
	,	_creditsTitleLabel     = (initLabel (StaticString "Project Serenity") (bright green) Nothing) {_labelScale = 3}
	,	_creditsBackButton     = initMenuButton "<-      Back" (\_ -> Menu)
	,	_creditsALCLogoView    = initPictureView (StaticPicture (Scale 0.12 0.12 $ getPicture "ALC_logo" assets))
	,	_creditsMoreGamesLabel = (initLabel (StaticString "Coming soon from ALC:") white (Just black)) 
			& (labelScale .~ 1.4) 
			& (labelTextOffset .~ (10, 20))
	,	_creditsAGameName      = "A Game Name"
	,	_creditsAGameNameLabel = (initLabel (DynamicString $ aCredits.creditsAGameName) (bright green) Nothing) 
			& (labelScale .~ 1.4) 
			& (labelTextOffset .~ (10, 20))
	}

viewCredits :: CreditsState a => a -> View a
viewCredits a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	pictureView a (aCredits.creditsALCLogoView) ((190,650),(0,0))
	,	initView ((0, 38), (1024, 52)) <++ -- Coming soon
		[	initView ((0, 0), (1024, 52)) & (viewBackground .~ (Just black))
		,	label a (aCredits.creditsAGameNameLabel) ((650, 0), (100, 52))
		,	label a (aCredits.creditsMoreGamesLabel) ((0, 0), (250, 52))
		,	initView ((650, 0), (1024-650, 52)) & (viewBackground .~ (Just black))
		]
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	label a (aCredits.creditsTitleLabel) ((15,680),(220,30))
		,	button a (aCredits.creditsBackButton) aMode ((80, 50),(185,28))
		]
	]

timeCredits :: CreditsState a => Float -> a -> a
timeCredits dt = execState $ do
	t <- aCredits.creditsTime <+= (dt*90)
	aCredits.creditsAGameNameLabel.labelTextOffset .= (10 - t,20)

timeCreditsIO :: CreditsState a => Float -> StateT a IO ()
timeCreditsIO dt = do
	time <- use $ aCredits.creditsTime
	name <- use $ aCredits.creditsAGameName
	when (time > 400 + (fromIntegral $ length name * 11)) $ do
		name <- liftIO generateName
		aCredits.creditsAGameName .= name
		aCredits.creditsTime .= 0
		aCredits.creditsAGameNameLabel.labelTextOffset .= (10,20)
		numGames <- aCredits.creditsNumGamesShown <+= 1
		aCredits.creditsAGameNameLabel.labelColor .= (bright $ ownerIDColor numGames)

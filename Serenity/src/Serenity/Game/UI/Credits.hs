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
	{	_creditsTime             :: Float
	,	_creditsNumGamesShown    :: Int
	,	_creditsTitleLabel       :: Label a
	,	_creditsBackButton       :: Button a ApplicationMode
	,	_creditsALCLogoView      :: PictureView a
	,	_creditsMoreGamesLabel   :: Label a
	,	_creditsAGameName        :: String
	,	_creditsAGameNameLabel   :: Label a
	,	_creditsDescriptionTable :: Table a
	,	_creditsPortraitsTable   :: Table a
	}
makeLenses ''CreditsData

class AppState a => CreditsState a where
	aCredits :: Simple Lens a (CreditsData a)

initCreditsData :: CreditsState a => Assets -> CreditsData a
initCreditsData assets = CreditsData
	{	_creditsTime           = 1000
	,	_creditsNumGamesShown    = 1
	,	_creditsTitleLabel       = (initLabel (StaticString "Project Serenity") (bright green) Nothing) & (labelScale .~ 3)
	,	_creditsBackButton       = initMenuButton "<-      Back" (\_ -> Menu)
	,	_creditsALCLogoView      = initPictureView (StaticPicture (Scale 0.125 0.125 $ getPicture "ALC_logo" assets))
	,	_creditsMoreGamesLabel   = (initLabel (StaticString "More titles from ALC:") white (Just black)) 
			& (labelScale .~ 1.4) 
			& (labelTextOffset .~ (20, 20))
	,	_creditsAGameName        = "A Game Name"
	,	_creditsAGameNameLabel   = (initLabel (DynamicString $ aCredits.creditsAGameName) (bright green) Nothing) 
			& (labelScale .~ 1.4) 
			& (labelTextOffset .~ (10, 20))
	,	_creditsDescriptionTable = (initTable 20 Nothing)
	,	_creditsPortraitsTable   = (initTable 110 Nothing)
	}

viewCredits :: CreditsState a => a -> View a
viewCredits a = (initView ((0, 0), (1024, 750))) 
	{	_viewDepict = background (a^.aAssets)
	}	<++
	[	pictureView a (aCredits.creditsALCLogoView) ((198,650),(0,0))
	,	initView ((0, 38), (1024, 52)) <++ -- Coming soon
		[	initView ((0, 0), (1024, 52)) & (viewBackground .~ (Just black))
		,	label a (aCredits.creditsAGameNameLabel) ((650, 0), (100, 52))
		,	label a (aCredits.creditsMoreGamesLabel) ((0, 0), (230, 52))
		,	initView ((650, 0), (1024-650, 52)) & (viewBackground .~ (Just black))
		]
	,	(initBox ((680, 0), (345, 750))) <++ -- Sidebar
		[	label a (aCredits.creditsTitleLabel) ((15,680),(220,30))
		,	table a (aCredits.creditsDescriptionTable) (to $ \_ -> descriptionString) (descriptionLabel a) ((15,100),(220,450))
		,	button a (aCredits.creditsBackButton) aMode ((80, 50),(185,28))
		]
	,	table a (aCredits.creditsPortraitsTable) picturesLens (profileView a) ((7,30),(400,500))
	]

timeCredits :: CreditsState a => Float -> a -> a
timeCredits dt = execState $ do
	t <- aCredits.creditsTime <+= (dt*90)
	aCredits.creditsAGameNameLabel.labelTextOffset .= (10 - t,20)

timeCreditsIO :: CreditsState a => Float -> StateT a IO ()
timeCreditsIO dt = do
	time <- use $ aCredits.creditsTime
	name <- use $ aCredits.creditsAGameName
	when (time > 420 + (fromIntegral $ length name * 11)) $ do
		name <- liftIO generateName
		aCredits.creditsAGameName .= name
		aCredits.creditsTime .= 0
		aCredits.creditsAGameNameLabel.labelTextOffset .= (10,20)
		numGames <- aCredits.creditsNumGamesShown <+= 1
		aCredits.creditsAGameNameLabel.labelColor .= (bright $ ownerIDColor numGames)

picturesLens :: Getter a [(String, String, String)]
picturesLens = to (\_ -> list) where 
	list =
		[	("CreditsVic",   "Vic Smith",    "Team Leader, Developer, and Designer")
		,	("CreditsLaith", "Laith Alissa", "Project Manager, Developer, and Artist")
		,	("CreditsJon",   "Jon Cave",     "Developer, Security Expert, and Quality Control")
		,	("CreditsJoe",   "Joe Siddall",  "Developer, Software Librarian, and Resident Squid")
		]

profileView :: CreditsState a => a -> (String, String, String) -> View a
profileView a (asset, name, description) = initView ((0,0),(100,600)) <++
	[	pictureViewStatic a (initPictureView (StaticPicture (Scale 0.12 0.12 $ getPicture asset (a^.aAssets)))) ((50,50),(0,0))
	,	labelStatic a (initLabel (StaticString name)        white Nothing & (labelScale .~ 2.9)) ((100,45),(0,0))
	,	labelStatic a (initLabel (StaticString description) white Nothing & (labelScale .~ 1.2)) ((100,15),(0,0))
	]

descriptionString = 
	[	"Project Serenity is a fourth year Computer"
	,	"Science project, developed by students at the"
	,	"University of Warwick as part of a MEng"
	,	"qualification. The game itself is written"
	,	"entirely in the pure functional programming"
	,	"language Haskell, and represents an examination"
	,	"of the suitability of the functional paradigm"
	,	"for game development."
	]

descriptionLabel a string = labelStatic a (initLabel (StaticString string) (dark white) Nothing) ((0,0),(0,0))

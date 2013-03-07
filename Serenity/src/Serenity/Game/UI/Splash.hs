{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Serenity.Game.UI.Splash where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Maths.Util

import Prelude hiding ((.), id)
import Control.Wire hiding (when)
import Control.Lens
import Control.Monad.State
import Control.Monad.Identity (Identity, runIdentity)
import GHC.Float

-- | Splash screen state.
data SplashData a = SplashData
	{	_splashTime :: Float
	,	_splashWire :: Wire () Identity (SplashData a) (SplashData a)
	,	_splashBlueBackground :: Label a
	,	_splashPresentsLabel  :: Label a
	,	_splashALCLogoPictureView :: PictureView a
	,	_splashSquids :: [ObjectState (Double, Double)]
	,	_splashTarget :: (Double, Double)
	}
makeLenses ''SplashData

class AppState a => SplashState a where
	aSplash :: Simple Lens a (SplashData a)

-- | Initialise splash screen state.
initSplashData :: SplashState a => Assets -> SplashData a
initSplashData assets = SplashData
	{	_splashTime = 0
	,	_splashWire = accelerateSquids
	,	_splashBlueBackground = initLabel NoString green Nothing
	,	_splashPresentsLabel  = (initLabel (StaticString "present...") white Nothing){_labelScale=0}
	,	_splashALCLogoPictureView = initPictureView (StaticPicture (Scale 0.1 0.1 $ getPicture "ALC_logo" assets))
	,	_splashSquids = 
		[	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		,	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		,	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		]
	,	_splashTarget = (512,400)
	}

-- | Main view for splash screen, dependent on various lenses on the application state a.
viewSplash :: SplashState a => a -> View a
viewSplash a = 
	(initView ((0, 0), (1024, 768))) 
	{	_viewDepict = depictSquids (getPicture "squid" (a^.aAssets)) (a^.aSplash.splashSquids)
	,	_viewDepictMode = ViewDepictModeViewUppermost
	,	_viewEventHandler = Just $ eventHandler a 
	} <++
	[	pictureView a (aSplash.splashALCLogoPictureView) ((520,400),(0,0))
	,	label       a (aSplash.splashBlueBackground)     ((0,0),(1024,750))
	,	label       a (aSplash.splashPresentsLabel)      ((450,100),(0,0))
	,	(initView ((0, 0), (1024, 768))) {_viewEventHandler = Just $ eventHandler a}
	] where
	eventHandler a (UIEventMouseUpInside _ _ _) = aMode.~Menu $ a
	eventHandler a (UIEventKeyPress _ _ _) = aMode.~Menu $ a
	eventHandler a (UIEventMotion r) = aSplash.splashTarget.~(pFloat2Double r) $ a
	eventHandler a _ = a

-- | Time evolution of splash screen, dependant on various lenses on the application state a.
timeSplash :: SplashState a => Float -> a -> a
timeSplash dt = execState $ do
	time <- aSplash.splashTime <+= dt
	when (time > 12 ) $ aMode .= Menu
	when (time > 5.5) $ aSplash %= runSplashWire dt
	aSplash.splashBlueBackground.labelBackground .= overlayColor time
	aSplash.splashALCLogoPictureView.pictureViewScale .= logoScale time
	aSplash.splashPresentsLabel.labelScale .= presentsScale time

logoScale time = 1 + time/5

presentsScale time = case time of
	time | time < 6 -> 0
	_ -> 2 

overlayColor time = Just $ makeColor navy_r navy_g navy_b t where
	t | time < 4  = 0
	  | time < 6  = (time-4)/2
	  | otherwise = 1
	(navy_r, navy_g, navy_b, _) = rgbaOfColor (mixColors 0.3 0.7 blue black)

----------------------- The Squid Comes Home -------------------

-- | Draw the squids
depictSquids squidAsset = Just . Pictures . (map (depictSquid squidAsset))

depictSquid squidAsset ObjectState{objPosition=(x,y), objVelocity=r} = 
	Translate (double2Float x) (double2Float y) (rotate (vectorToAngle r) $ squidAsset)

vectorToAngle (0, 0) = 0
vectorToAngle (x, y) = ((atan2 (double2Float x) (double2Float y))/pi * 180)

-- | Use netwire to accelerate the squids toward the target.
runSplashWire delta splash = newWorld where 
	(result, newWire) = runIdentity $ stepWire (splash^.splashWire) (float2Double delta) splash
	updateWire = splashWire .~ newWire
	newWorld = case result of
		Right w -> updateWire w
		Left _ -> updateWire splash

accelerateSquids = proc splash -> do
	newSquids <- mapArrowWithIndexFrom 0 accelerateSquid -< zip (splash^.splashSquids) (repeat $ splash^.splashTarget)
	id -<  splashSquids .~ newSquids $ splash 

mapArrowWithIndexFrom i arrow = proc list -> do
	case list of
		(x:xs) -> do
			y <- arrow i -< x
			ys <- mapArrowWithIndexFrom (i+1) arrow -< xs
			id -< y:ys
		[] -> id -< []

accelerateSquid i = proc (squid, target) -> do 
	newState <- object_ (\_ -> id) (startState i) -< (Accelerate ((target- (targetOffset i)) - (objPosition squid)), 0.2)
	id -< newState

startState i = case i of
	0 -> ObjectState {objPosition = (-20, -20), objVelocity = (10,0)}
	1 -> ObjectState {objPosition = (512, -20), objVelocity = (0,10)}
	2 -> ObjectState {objPosition = (1024,-20), objVelocity = (10,0)}
	_ -> ObjectState {objPosition = (500, 800), objVelocity = (10,0)}

targetOffset i = case i of
	0 -> (-100,-100)
	1 -> (0,0)
	2 -> (100,-100)
	_ -> (0,0)
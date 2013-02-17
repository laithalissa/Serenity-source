{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Serenity.Game.UI.Splash where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External
import Serenity.Maths.Util

import Prelude hiding ((.), id)
import Control.Wire
import Control.Lens
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

-- | Initialise splash screen state.
initSplashData :: Assets -> SplashData a
initSplashData assets = SplashData
	{	_splashTime = 0
	,	_splashWire = squidsWire
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
viewSplash :: a -> Simple Lens a (SplashData a) -> Simple Lens a Assets -> Simple Lens a ApplicationMode -> View a
viewSplash a aSplash aAssets aMode = 
	(initView ((0, 0), (1024, 768))) 
	{	_viewDepict = depictSquids (getPicture "squid" (a^.aAssets)) (a^.aSplash.splashSquids)
	,	_viewDepictMode = ViewDepictModeViewUppermost
	,	_viewEventHandler = Just $ eventHandler a 
	} <++
	[	pictureView a (aSplash.splashALCLogoPictureView) ((520,400),(0,0))
	,	label       a (aSplash.splashBlueBackground)     ((0,0),(1024,750))
	,	label       a (aSplash.splashPresentsLabel)      ((450,100),(0,0))
	] where
	eventHandler a (UIEventKey _ _ _ _) = aMode.~Menu $ a
	eventHandler a (UIEventMotion r)    = aSplash.splashTarget.~(pFloat2Double r) $ a
	eventHandler a _ = a

-- | Time evolution of splash screen, dependant on various lenses on the application state a.
timeSplash :: Simple Lens a (SplashData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeSplash aSplash aMode dt a = (squidLogic $ a^.aSplash.splashWire).alpha.scale1.scale2.mode.time $ a 
	where
		time  = aSplash.splashTime +~ dt
		mode  = if a^.aSplash.splashTime < 12 then id else aMode.~Menu
		alpha = aSplash.splashBlueBackground.labelBackground .~ overlayColor (a^.aSplash.splashTime)
		scale1 = aSplash.splashALCLogoPictureView.pictureViewScale .~ logoScale (a^.aSplash.splashTime)
		scale2 = aSplash.splashPresentsLabel.labelScale .~ presentsScale (a^.aSplash.splashTime)
		squidLogic wire a = 
			if (a^.aSplash.splashTime) > 5.5 
				then aSplash.splashWire .~ wire' $ aSplash .~ splash' $ a  
				else a
			where (splash', wire') = updateOnTime dt (a^.aSplash)

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

depictSquids squidAsset = Just . Pictures . (map (depictSquid squidAsset))

depictSquid squidAsset ObjectState{objPosition=(x,y), objVelocity=r} = 
	Translate (double2Float x) (double2Float y) (rotate (vectorToAngle r) $ squidAsset)

vectorToAngle (0, 0) = 0
vectorToAngle (x, y) = ((atan2 (double2Float x) (double2Float y))/pi * 180)

updateOnTime delta splash = (newWorld, newWire) where 
	(result, newWire) = runIdentity $ stepWire (splash^.splashWire) (float2Double delta) splash
	newWorld = case result of
		Right w -> w
		Left _ -> splash

squidsWire = proc splash -> do
	newSquids <- mapArrowWithIndexFrom 0 squidState -< zip (splash^.splashSquids) (repeat $ splash^.splashTarget)
	id -<  splashSquids .~ newSquids $ splash 

mapArrowWithIndexFrom i arrow = proc list -> do
	case list of
		(x:xs) -> do
			y <- arrow i -< x
			ys <- mapArrowWithIndexFrom (i+1) arrow -< xs
			id -< y:ys
		[] -> id -< []

squidState i = proc (squid, target) -> do 
	--t <- time -< ()
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
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Serenity.Game.UI.Splash where

import Serenity.Sheen
import Serenity.Game.UI.Application
import Serenity.External

import Prelude hiding ((.), id)
import Control.Wire
import Control.Lens
import Control.Monad.Identity (Identity, runIdentity)
import GHC.Float

data SplashData a = SplashData
	{	_splashTime :: Float
	,	_splashWire :: Wire () Identity (SplashData a) (SplashData a)
	,	_splashLabel :: Label a
	,	_splashALCLogoPictureView :: PictureView a
	,	_splashSquids :: [ObjectState (Double, Double)]
	,	_splashTarget :: (Double, Double)
	}

makeLenses ''SplashData

initSplashData :: Simple Lens a (SplashData a) -> Assets -> SplashData a
initSplashData aSplash assets = SplashData
	{	_splashTime = 0
	,	_splashWire = squidsWire
	,	_splashLabel = initLabel NoString green Nothing
	,	_splashALCLogoPictureView = initPictureView (StaticPicture (Scale 0.1 0.1 $ getPicture "ALC_logo" assets))
	,	_splashSquids = 
		[	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		,	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		,	ObjectState {objPosition = (-200,-200), objVelocity = (0,0)}
		]
	,	_splashTarget = (512,400)
	}

viewSplash :: a -> Simple Lens a (SplashData a) -> Simple Lens a Assets -> View a
viewSplash a aSplash aAssets = 
	(initView ((0, 0), (1024, 768))) 
	{	_viewDepict = depictSquids (getPicture "squid" (a^.aAssets)) (a^.aSplash.splashSquids)
	,	_viewDepictMode = ViewDepictModeViewUppermost
	}	<++
	[	pictureView a (aSplash.splashALCLogoPictureView) ((520,400),(0,0))
	,	label       a (aSplash.splashLabel)              ((0,0),(1024,768))
	]

timeSplash :: Simple Lens a (SplashData a) -> Simple Lens a ApplicationMode -> Float -> a -> a
timeSplash aSplash aMode dt a = (squidLogic $ a^.aSplash.splashWire).alpha.scale.mode.time $ a 
	where
		time  = aSplash.splashTime +~ dt
		mode  = if a^.aSplash.splashTime < 12 then id else aMode.~Menu
		alpha = aSplash.splashLabel.labelBackground .~ overlayColor (a^.aSplash.splashTime)
		scale = aSplash.splashALCLogoPictureView.pictureViewScale .~ logoScale (a^.aSplash.splashTime)
		squidLogic wire a = 
			if (a^.aSplash.splashTime) > 5.5 
				then aSplash.splashWire .~ wire' $ aSplash .~ splash' $ a  
				else a
			where (splash', wire') = updateOnTime dt (a^.aSplash)

logoScale time = 1 + time/5

overlayColor time = Just $ makeColor navy_r navy_g navy_b t where
	t | time < 4  = 0
	  | time < 6  = (time - 4)/2
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
	newState <- object_ (\_ -> id) (startState i) -< (Accelerate (target - (objPosition squid)), 0.2)
	id -< newState

startState i = case i of
	0 -> ObjectState {objPosition = (-20,-20), objVelocity = (10,0)}
	1 -> ObjectState {objPosition = (-20,500), objVelocity = (0,10)}
	2 -> ObjectState {objPosition = (500,800), objVelocity = (10,0)}
	_ -> ObjectState {objPosition = (500,800), objVelocity = (10,0)}

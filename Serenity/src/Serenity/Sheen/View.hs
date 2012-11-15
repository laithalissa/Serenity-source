module Serenity.Sheen.View
(	View (..)
,	makeView
,	draw
,	click
,	playIOZero
)
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing, catMaybes)
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM)

type Size = (Float, Float)
type Rect = (Size, Size)

data View world = View 
	{	subviews        :: [View world]
	,	frame           :: Rect
	,	zPosition      :: Int
	,	backgroundFill :: Maybe Color
	,	depict          :: Maybe (Size -> world -> Picture)
	,	respond         :: Maybe (Event -> world -> world)
	}

makeView :: Size -> Size -> View world
makeView width height = View 
	{	frame = (width, height)
	,	subviews = []
	,	zPosition = 0
	,	backgroundFill = Nothing
	,	depict = Nothing
	,	respond = Nothing
	}

draw :: View world -> world -> Picture
draw view@View{subviews=subs, frame=((x,y),(width,height)), backgroundFill=c, depict=d} world = 
	Translate x y $ Pictures $ background c ++ self d ++ children where
		self (Just dep) = [dep (width,height) world]
		self Nothing = []
		background (Just color) = [coloredPolygon color (width, height)]
		background Nothing = []
		children = map (\view -> draw view world) orderedSubs
		orderedSubs = sortBy (comparing zPosition) subs

coloredPolygon :: Color -> Size -> Picture
coloredPolygon c (width, height) = Color c $ Polygon [(0,0), (0, height), (width, height), (width, 0)]

click :: Event -> View world -> Maybe (world -> world)
click (EventMotion _) _ = Nothing
click event@(EventKey _ _ _ (eventX,eventY)) view@View{subviews=subs, frame=((viewX,viewY),(viewWidth,viewHeight))} =
	if clickInBounds 
		then case subviewResponses of 
			aResponse:_ -> Just aResponse 
			[]           -> respond view <*> (Just event)
		else Nothing 
	where
		clickInBounds = and [viewX<eventX, eventX<viewX+viewWidth, viewY<eventY, eventY<viewY+viewHeight]
		orderedSubs = sortBy (comparing (negate . zPosition)) subs
		subviewResponses = catMaybes $ map (click $ translateEvent (-viewX) (-viewY) event) orderedSubs

playIOZero display@(InWindow windowName (sizeX, sizeY) position) color steps initialWorld depict respond evolve = 
	playIO display color steps initialWorld 
	(\world -> do x <- depict world; return $ Translate (fromIntegral $ -sizeX `div` 2) (fromIntegral $ -sizeY `div` 2) x;)
	(\event -> \world -> respond (translateEvent (fromIntegral $ sizeX `div` 2) (fromIntegral $ sizeY `div` 2) event) world)
	evolve 

translateEvent x y (EventKey key state modifiers (eventX, eventY)) = (EventKey key state modifiers (eventX + x, eventY + y))
translateEvent x y (EventMotion (eventX, eventY)) = (EventMotion (eventX + x, eventY + y))

----------------------------------------------------------------------------------------------------------------------------
-- Example --

main = playIOZero
	(InWindow "ViewTest" (500, 500) (0,40)) black 1 0
	(\world -> return $ draw mainView world) 
	(\event -> \world -> case (click event mainView) of Nothing -> return world; Just function -> return $ function world)
	(\float -> \world -> return world)

mainView = (makeView (0,0) (500,500))
	{	subviews = [secondaryView1, secondaryView2, secondaryView3]
	,	backgroundFill = Just yellow
	,	depict   = Just $ \(width,height) -> \word -> Translate 100 100 $ Text (show word)
	}

secondaryView1 = (makeView (400,400) (100,100))
	{	subviews = [secondaryView2]
	,	backgroundFill = Just blue
	,	respond = Just (\event -> \world -> world + 5)
	}

secondaryView2 = (makeView (0,0) (100,50))
	{	subviews = [circleView1, circleView2]
	,	backgroundFill = Just green
	,	respond = Just (\event -> \world -> world + 50)
	}

secondaryView3 = (makeView (380,380) (40,40))
	{	backgroundFill = Just red
	,	zPosition = -1
	,	respond = Just (\event -> \world -> world + 500)
	}

circleView1 = (makeView (33,25) (10,10))
	{	depict   = Just $ \(width,height) -> \word -> Color red $ Translate 5 5 $ ThickCircle 5 10
	,	respond = Just (\event -> \world -> world + 5000)
	}

circleView2 = (makeView (66,25) (10, 10))
	{	depict   = Just $ \(width,height) -> \word -> Color yellow $ Translate 5 5 $ ThickCircle 5 10
	}
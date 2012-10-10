module Serenity.Sheen.View (
	View (..)
,	make_view
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
	,	z_position      :: Int
	,	background_fill :: Maybe Color
	,	depict          :: Maybe (Size -> world -> Picture)
	,	respond         :: Maybe (Event -> world -> world)
	}

make_view :: Size -> Size -> View world
make_view width height = View 
	{	frame = (width, height)
	,	subviews = []
	,	z_position = 0
	,	background_fill = Nothing
	,	depict = Nothing
	,	respond = Nothing
	}

draw :: View world -> world -> Picture
draw view@View{subviews=subs, frame=((x,y),(width,height)), background_fill=c, depict=d} world = 
	Translate x y $ Pictures $ background c ++ self d ++ children where
		self (Just dep) = [dep (width,height) world]
		self Nothing = []
		background (Just color) = [colored_polygon color (width, height)]
		background Nothing = []
		children = map (\view -> draw view world) ordered_subs
		ordered_subs = sortBy (comparing z_position) subs

colored_polygon :: Color -> Size -> Picture
colored_polygon c (width, height) = Color c $ Polygon [(0,0), (0, height), (width, height), (width, 0)]

click :: Event -> View world -> Maybe (world -> world)
click (EventMotion _) _ = Nothing
click event@(EventKey _ _ _ (event_x,event_y)) view@View{subviews=subs, frame=((view_x,view_y),(view_width,view_height))} =
	if click_in_bounds 
		then case subview_responses of 
			a_response:_ -> Just a_response 
			[]           -> respond view <*> (Just event)
		else Nothing 
	where
		click_in_bounds = and [view_x<event_x, event_x<view_x+view_width, view_y<event_y, event_y<view_y+view_height]
		ordered_subs = sortBy (comparing (negate . z_position)) subs
		subview_responses = catMaybes $ map (click $ translate_event (-view_x) (-view_y) event) ordered_subs

playIOZero display@(InWindow window_name (size_x, size_y) position) color steps initial_world depict respond evolve = 
	playIO display color steps initial_world 
	(\world -> do x <- depict world; return $ Translate (fromIntegral $ -size_x `div` 2) (fromIntegral $ -size_y `div` 2) x;)
	(\event -> \world -> respond (translate_event (fromIntegral $ size_x `div` 2) (fromIntegral $ size_y `div` 2) event) world)
	evolve 

translate_event x y (EventKey key state modifiers (event_x, event_y)) = (EventKey key state modifiers (event_x + x, event_y + y))
translate_event x y (EventMotion (event_x, event_y)) = (EventMotion (event_x + x, event_y + y))

----------------------------------------------------------------------------------------------------------------------------
-- Example --

main = playIOZero
	(InWindow "ViewTest" (500, 500) (0,40)) black 1 0
	(\world -> return $ draw mainView world) 
	(\event -> \world -> case (click event mainView) of Nothing -> return world; Just function -> return $ function world)
	(\float -> \world -> return world)

mainView = (make_view (0,0) (500,500))
	{	subviews = [secondaryView1, secondaryView2, secondaryView3]
	,	background_fill = Just yellow
	,	depict   = Just $ \(width,height) -> \word -> Translate 100 100 $ Text (show word)
	}

secondaryView1 = (make_view (400,400) (100,100))
	{	subviews = [secondaryView2]
	,	background_fill = Just blue
	,	respond = Just (\event -> \world -> world + 5)
	}

secondaryView2 = (make_view (0,0) (100,50))
	{	subviews = [circleView1, circleView2]
	,	background_fill = Just green
	,	respond = Just (\event -> \world -> world + 50)
	}

secondaryView3 = (make_view (380,380) (40,40))
	{	background_fill = Just red
	,	z_position = -1
	,	respond = Just (\event -> \world -> world + 500)
	}

circleView1 = (make_view (33,25) (10,10))
	{	depict   = Just $ \(width,height) -> \word -> Color red $ Translate 5 5 $ ThickCircle 5 10
	,	respond = Just (\event -> \world -> world + 5000)
	}

circleView2 = (make_view (66,25) (10, 10))
	{	depict   = Just $ \(width,height) -> \word -> Color yellow $ Translate 5 5 $ ThickCircle 5 10
	}
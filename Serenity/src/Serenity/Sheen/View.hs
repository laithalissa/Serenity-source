module Serenity.Sheen.View
	( View (..)
	, makeView
	, drawView
	, handleViewEvent
	, changeView
	)
where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

import Serenity.Sheen.UIEvent
import Serenity.Sheen.Util

data View world = View 
	{ viewID :: String
	, subviews :: [View world]
	, frame :: Extent
	, zIndex :: Int
	, background :: Maybe Color
	, depict :: Maybe (world -> Picture)
	, eventHandler :: Maybe (UIEvent -> world -> world)
	}

instance Show (View a) where
	show (View { viewID = id, subviews = sub, background = bg }) = id ++ ": " ++ show bg ++ show (map show sub)

makeView :: (Int, Int, Int, Int) -> View world
makeView (xmin, xmax, ymin, ymax) = View 
	{ viewID = ""
	, frame = makeExtent ymax ymin xmax xmin
	, subviews = []
	, zIndex = 0
	, background = Nothing
	, depict = Nothing
	, eventHandler = Nothing
	}

drawView :: View world -> world -> Picture
drawView view world = Translate (fromIntegral xmin) (fromIntegral ymin) $ Pictures $ bg ++ pict ++ children
	where
	(ymax, ymin, xmax, xmin) = takeExtent $ frame view

	bg = case background view of
		Just colour -> [coloredRectangle colour (fromIntegral $ xmax - xmin, fromIntegral $ ymax - ymin)]
		Nothing -> []

	pict = case depict view of
		Just f -> [f world]
		Nothing -> []

	children = map (\v -> drawView v world) (orderViews $ subviews view)

handleViewEvent :: Event -> View world -> world -> world
handleViewEvent event view =
	case eventToUIEvent $ translateEvent (fromIntegral $ 1024 `div` 2) (fromIntegral $ 768 `div` 2) event of
		Just uiEvent -> case getEventHandler uiEvent view of
					Just handler -> handler uiEvent
					Nothing -> id
		Nothing -> id

getEventHandler :: UIEvent -> View world -> Maybe (UIEvent -> world -> world)
getEventHandler event@(ViewClick point _) view = 
	if eventInView then
		case subviewHandlers of
			[] -> eventHandler view
			hs -> Just (last hs)
	else
		Nothing

	where
	eventInView = pointInExtent (frame view) point
	subviewHandlers = catMaybes $ map (getEventHandler $ translateUIEvent (-xmin) (-ymin) event) $ orderViews (subviews view)
	(_, ymin, _, xmin) = takeExtent $ frame view

getEventHandler _ _ = Nothing

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing zIndex)

translateEvent :: Float -> Float -> Event -> Event
translateEvent x y (EventKey key state modifiers (eventX, eventY)) = (EventKey key state modifiers (eventX + x, eventY + y))
translateEvent x y (EventMotion (eventX, eventY)) = (EventMotion (eventX + x, eventY + y))

translateUIEvent :: Int -> Int -> UIEvent -> UIEvent
translateUIEvent x y (ViewClick (clickX, clickY) button) = ViewClick newPoint button
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))

changeView :: String -> (View a -> View a) -> View a -> View a
changeView id f view
	| viewID view == id = f view
	| otherwise = view { subviews = map (changeView id f) (subviews view) }

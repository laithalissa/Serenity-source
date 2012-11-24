module Serenity.Sheen.View
(	View (..)
,	makeView
,	drawView
,	handleViewEvent
,	changeView
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
	{	viewID :: String                   -- ^ Unique identifier for the view
	,	subviews :: [View world]           -- ^ List of views below this one
	,	frame :: Extent                    -- ^ Rectangular area the view represents
	,	zIndex :: Int                      -- ^ Stack position of the view
	,	background :: Maybe Color          -- ^ Background colour
	,	depict :: Maybe (world -> Picture) -- ^ Callback to draw world state within the view
	,	eventHandler :: Maybe (UIEvent -> world -> world) -- ^ Callback to handle UIEvents
	}

-- | Create new view
makeView ::
	String                  -- ^ Unique identifier for the view
	-> (Int, Int, Int, Int) -- ^ Coordinates of the view: (xmin, xmax, ymin, ymax)
	-> View world

makeView ident (xmin, xmax, ymin, ymax) = View
	{	viewID = ident
	,	frame = makeExtent ymax ymin xmax xmin
	,	subviews = []
	,	zIndex = 0
	,	background = Nothing
	,	depict = Nothing
	,	eventHandler = Nothing
	}

-- | Draw a view hierarchy
drawView ::
	View world -- ^ Root of the hierarchy to draw
	-> world   -- ^ The world state to be used during drawing
	-> Picture
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

-- | Handle a Gloss event
-- This function gets the event handler from the deepest view in the
-- hierarchy that is within the scope of the event and applies it to
-- the given world state
handleViewEvent ::
	Event         -- ^ The event to react to
	-> View world -- ^ View hierarchy
	-> world      -- ^ Current world state
	-> world
handleViewEvent event view =
	case eventToUIEvent event of
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

-- | Change something about a specific view in a view hierarchy
changeView ::
	String                -- ^ The ID of the view to change
	-> (View a -> View a) -- ^ Function that performs the change
	-> View a             -- ^ View hierarchy containing the target view
	-> View a
changeView id f view
	| viewID view == id = f view
	| otherwise = view { subviews = map (changeView id f) (subviews view) }

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing zIndex)

translateUIEvent :: Int -> Int -> UIEvent -> UIEvent
translateUIEvent x y (ViewClick (clickX, clickY) button) = ViewClick newPoint button
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))


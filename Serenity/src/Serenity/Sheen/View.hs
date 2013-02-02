{-# LANGUAGE TemplateHaskell #-}

module Serenity.Sheen.View
(	View (..)
,	makeView
,	drawView
,	handleViewEvent
,	changeView
)
where

import Serenity.Sheen.UIEvent
import Serenity.Sheen.Util

import Control.Lens
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game

data View a = View
	{	_viewID           :: String                    -- ^ Unique identifier for the view
	,	_viewSubviews     :: [View a]                  -- ^ List of views below this one
	,	_viewFrame        :: Extent                    -- ^ Rectangular area the view represents
	,	_viewZIndex       :: Int                       -- ^ Stack position of the view
	,	_viewBackground   :: Maybe Color               -- ^ Background colour
	,	_viewDepict       :: Maybe (a -> Picture)      -- ^ Callback to draw the state within the view
	,	_viewEventHandler :: Maybe (UIEvent -> a -> a) -- ^ Callback to handle UIEvents
	}
makeLenses ''View

-- | Create new view
makeView ::
	String                  -- ^ Unique identifier for the view
	-> (Int, Int, Int, Int) -- ^ Coordinates of the view: (xmin, xmax, ymin, ymax)
	-> View world

makeView ident (xmin, xmax, ymin, ymax) = View
	{	_viewID = ident
	,	_viewFrame = makeExtent ymax ymin xmax xmin
	,	_viewSubviews = []
	,	_viewZIndex = 0
	,	_viewBackground = Nothing
	,	_viewDepict = Nothing
	,	_viewEventHandler = Nothing
	}

-- | Draw a view hierarchy
drawView ::
	View a -- ^ Root of the hierarchy to draw
	-> a   -- ^ The world state to be used during drawing
	-> Picture
drawView view world = Translate (fromIntegral xmin) (fromIntegral ymin) $ Pictures $ background ++ pict ++ children
	where
	(ymax, ymin, xmax, xmin) = takeExtent $ view^.viewFrame

	background = case view^.viewBackground of
		Just colour -> [coloredRectangle colour (fromIntegral $ xmax - xmin, fromIntegral $ ymax - ymin)]
		Nothing -> []

	pict = case view^.viewDepict of
		Just f -> [f world]
		Nothing -> []

	children = map (\v -> drawView v world) (orderViews $ _viewSubviews view)

-- | Handle a Gloss event
-- This function gets the event handler from the deepest view in the
-- hierarchy that is within the scope of the event and applies it to
-- the given world state
handleViewEvent ::
	Event         -- ^ The event to react to
	-> View world -- ^ View hierarchy
	-> world      -- ^ Current world state
	-> world
handleViewEvent event view = case eventToUIEvent event of
	Just uiEvent -> case getEventHandler uiEvent view of
		Just handler -> handler uiEvent
		Nothing -> id
	Nothing -> id

getEventHandler :: UIEvent -> View world -> Maybe (UIEvent -> world -> world)
getEventHandler event@(ViewClick point _) view =
	if eventInView then
		case subviewHandlers of
			[] -> view^.viewEventHandler
			hs -> Just (last hs)
	else
		Nothing
	where
	eventInView = pointInExtent (view^.viewFrame) point
	subviewHandlers = catMaybes $ map (getEventHandler $ translateUIEvent (-xmin) (-ymin) event) $ orderViews $ view^.viewSubviews
	(_, ymin, _, xmin) = takeExtent $ view^.viewFrame
getEventHandler _ _ = Nothing

-- | Change something about a specific view in a view hierarchy
changeView ::
	String                -- ^ The ID of the view to change
	-> (View a -> View a) -- ^ Function that performs the change
	-> View a             -- ^ View hierarchy containing the target view
	-> View a
changeView id f view
	| view^.viewID == id = f view
	| otherwise = view { _viewSubviews = map (changeView id f) (_viewSubviews view) }

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing (^.viewZIndex))

translateUIEvent :: Int -> Int -> UIEvent -> UIEvent
translateUIEvent x y (ViewClick (clickX, clickY) button) = ViewClick newPoint button
	where newPoint = (clickX + (fromIntegral x), clickY + (fromIntegral y))


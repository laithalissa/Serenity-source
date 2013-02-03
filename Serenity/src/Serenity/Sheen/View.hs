{-# LANGUAGE TemplateHaskell #-}

module Serenity.Sheen.View where

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
	{	_viewSubviews     :: [View a]                  -- ^ List of views below this one
	,	_viewFrame        :: Extent                    -- ^ Rectangular area the view represents
	,	_viewZIndex       :: Int                       -- ^ Stack position of the view
	,	_viewBackground   :: Maybe Color               -- ^ Background colour
	,	_viewDepict       :: Maybe (a -> Picture)      -- ^ Callback to draw the state within the view
	,	_viewEventHandler :: Maybe (UIEvent -> a -> a) -- ^ Callback to handle UIEvents
	}
makeLenses ''View

class ViewController a where
	getView :: View a
	draw :: a -> Picture
	draw = drawView getView
	handleEvent :: Event -> a -> a
	handleEvent event a = handleViewEvent (event2UIEvent event) getView a

-- | Create new view
initView
	:: (Int, Int, Int, Int) -- ^ Coordinates of the view: (xmin, xmax, ymin, ymax)
	-> View world

initView (xmin, xmax, ymin, ymax) = View
	{	_viewFrame = makeExtent ymax ymin xmax xmin
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
	UIEvent     -- ^ The event to react to
	-> View a -- ^ View hierarchy
	-> a      -- ^ Current state
	-> a      -- ^ New state
handleViewEvent event view = case getEventHandler event view of
	Just handler -> handler event
	Nothing -> id

getEventHandler :: UIEvent -> View a -> Maybe (UIEvent -> a -> a)
getEventHandler event@(UIEventKey point _ _ _) view =
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
--getEventHandler event@(UIEventMotion point) view =
getEventHandler _ _ = Nothing

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing (^.viewZIndex))


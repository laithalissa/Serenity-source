{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Serenity.Sheen.View where

import Serenity.Sheen.UIEvent
import Serenity.Sheen.Util

import Control.Lens
import Control.Monad.State
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Data.Monoid

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.IO.Game

data View a = View
	{	_viewSubviews     :: [View a]             -- ^ List of views below this one.
	,	_viewFrame        :: Extent               -- ^ Rectangular area the view represents.
	,	_viewZIndex       :: Int                  -- ^ Stack position of the view.
	,	_viewBackground   :: Maybe Color          -- ^ Background colour.
	,	_viewDepict       :: Maybe Picture        -- ^ Gloss picture to draw contents of view.
	,	_viewDepictMode   :: ViewDepictMode       -- ^ Whether to draw subviews or own view depiction uppermost.
	,	_viewEventHandler :: Maybe (UIEvent -> a) -- ^ Callback to handle UIEvents.
	}

data ViewDepictMode = ViewDepictModeSubviewsUppermost | ViewDepictModeViewUppermost

makeLenses ''View

type ViewLens a = Simple Lens (View a) (View a)

type IndexPath = [Int]

--lensAtIndexPath :: [Int] -> ViewLens a
lensAtIndexPath path = foldr (.) id (map (\i -> viewSubviews.ix i) path)

data ViewGlobals a = ViewGlobals
	{	_globalMouseOver :: IndexPath
	,	_globalFocus     :: IndexPath
	}
makeLenses ''ViewGlobals

initGlobals = ViewGlobals
	{	_globalMouseOver = []
	,	_globalFocus     = []
	}

class ViewController a where
	globals :: Simple Lens a (ViewGlobals a)
	getView :: a -> View a
	updateTime :: Float -> a -> a
	updateTime _ = id

class Drawable a where
	render :: a -> Picture

draw :: ViewController a => a -> Picture
draw a = drawView (getView a)

handleEvent :: ViewController a => Event -> a -> a
handleEvent event a = handleViewEvent (event2UIEvent event) (getView a) a

-- | Create new view
initView
	:: ((Int, Int), (Int, Int)) -- ^ Coordinates of the view: (xmin, xmax, ymin, ymax)
	-> View world

initView ((xmin, ymin), (xsize, ysize)) = View
	{	_viewFrame = makeExtent (ymin+ysize) ymin (xmin+xsize) xmin
	,	_viewSubviews = []
	,	_viewZIndex = 0
	,	_viewBackground = Nothing
	,	_viewDepict = Nothing
	,	_viewDepictMode = ViewDepictModeSubviewsUppermost
	,	_viewEventHandler = Nothing
	}

-- | Draw a view hierarchy
drawView ::
	View a -- ^ Root of the hierarchy to draw
	-> Picture
drawView view = Translate (fromIntegral xmin) (fromIntegral ymin) $ Pictures $ pictures
	where
	pictures = case view^.viewDepictMode of
		ViewDepictModeSubviewsUppermost -> background ++ pict ++ children
		ViewDepictModeViewUppermost     -> background ++ children ++ pict
	(ymax, ymin, xmax, xmin) = takeExtent $ view^.viewFrame
	background = case view^.viewBackground of
		Just colour -> [coloredRectangle colour (fromIntegral $ xmax - xmin, fromIntegral $ ymax - ymin)]
		Nothing -> []
	pict = case view^.viewDepict of
		Just f -> [f]
		Nothing -> []
	children = map (\v -> drawView v) (orderViews $ _viewSubviews view)

-- | Handle a Gloss event
-- This function gets the event handler from the deepest view in the hierarchy that is within the scope of the event 
-- and applies it to the given world state.
handleViewEvent 
	:: ViewController a
	=> UIEvent   -- ^ The event to react to
	-> View a -- ^ View hierarchy
	-> a      -- ^ Old state
	-> a      -- ^ New state
handleViewEvent event@(UIEventKey point (MouseButton _) _ _) view = execState $ do
	oldIndexPath <- use $ globals.globalFocus
	newIndexPath <- case getEventHandler point view of
		Just (handler, indexPath) -> do put $ handler event; return indexPath
		Nothing -> return []
	when  (oldIndexPath /= newIndexPath) $ do
		sendEvent UIEventFocusLost oldIndexPath
		sendEvent UIEventFocusGained newIndexPath
	globals.globalFocus .= newIndexPath

handleViewEvent event@(UIEventKey _ _ _ _) view = execState $ do
	indexPath <- use $ globals.globalFocus
	handler <- return $ view^?(lensAtIndexPath indexPath).viewEventHandler
	case join handler of
		Just h -> put $ h event
		Nothing -> return ()

handleViewEvent event@(UIEventMotion point) view = execState $ do
	oldIndexPath <- use $ globals.globalMouseOver
	newIndexPath <- case getEventHandler point view of
		Just (handler, indexPath) -> do put $ handler event; return indexPath
		Nothing -> return []
	when (oldIndexPath /= newIndexPath) $ do
		sendEvent UIEventMouseOverOutside oldIndexPath
		sendEvent UIEventMouseOverInside newIndexPath
	globals.globalMouseOver .= newIndexPath

handleViewEvent _ _ = id

sendEvent event indexPath = do
	a <- get
	handler <- return $ (getView a)^?(lensAtIndexPath indexPath).viewEventHandler
	case join handler of
		Just h -> put $ h event
		Nothing -> return ()

getEventHandler :: (Float, Float) -> View a -> Maybe (UIEvent -> a, IndexPath)
getEventHandler = getEventHandler' []

getEventHandler' :: IndexPath -> (Float, Float) -> View a -> Maybe (UIEvent -> a, IndexPath)
getEventHandler' currentPath point@(x,y) view =
	if eventInView 
		then case catMaybes subviewHandlers of
			[] -> do eventHandler <- view^.viewEventHandler; return (eventHandler, currentPath)
			hs -> Just (last hs)
		else Nothing
	where
	eventInView = pointInExtent (view^.viewFrame) point
	subviewHandlers = map (\(i,v) -> getEventHandler' (currentPath++[i]) (x-(fromIntegral xmin), y-(fromIntegral ymin)) v) (zip [0..] $ subviews)
	subviews = orderViews $ view^.viewSubviews
	(_, ymin, _, xmin) = takeExtent $ view^.viewFrame

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing (^.viewZIndex))

-- | Append subviews.
(<++) :: View a -> [View a] -> View a
(<++) a b = viewSubviews %~ (++b) $ a
infixl 1 <++

instance Monoid (View a) where
	mempty = initView ((0,0),(0,0))
	mappend a b = viewSubviews %~ (++[b]) $ a

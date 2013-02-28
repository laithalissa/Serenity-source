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
	,	_globalMouseDown :: IndexPath
	,	_globalFocus     :: IndexPath
	}
makeLenses ''ViewGlobals

initGlobals = ViewGlobals
	{	_globalMouseOver = []
	,	_globalMouseDown = []
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
handleEvent event = handleViewEvents (event2UIEvents event)

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

handleViewEvents :: ViewController a => ((Float, Float), UIEvents) -> a -> a
handleViewEvents (point, events) = execState $ do
	a <- get
	indexPathUnderMouse <- return $ indexPathAtPoint point (getView a)
	indexPathLastMouseDown <- use (globals.globalMouseDown)
	indexPathFocus <- use (globals.globalFocus)
	when (events^.uieventsUpdateFocus)        $ updateFocus indexPathUnderMouse
	when (events^.uieventsUpdateMouseOver)    $ updateMouseOver indexPathUnderMouse
	forM (events^.uieventsEventUnderMouse)    $ sendEvent indexPathUnderMouse
	forM (events^.uieventsEventToFocus)       $ sendEvent indexPathFocus
	when (indexPathUnderMouse /= indexPathLastMouseDown) $ do
		forM (events^.uieventsEventLastMouseDown) $ sendEvent indexPathLastMouseDown; return ()
	when (events^.uieventsUpdateMouseDown)    $ globals.globalMouseDown .= indexPathUnderMouse

updateFocus :: ViewController a => IndexPath -> State a ()
updateFocus = updateGlobalsWith globalFocus UIEventFocusLost UIEventFocusGained    

updateMouseOver :: ViewController a => IndexPath -> State a ()
updateMouseOver = updateGlobalsWith globalMouseOver UIEventMouseOverOutside UIEventMouseOverInside

updateGlobalsWith :: ViewController a => SimpleLens (ViewGlobals a) IndexPath -> UIEvent -> UIEvent -> IndexPath -> State a ()
updateGlobalsWith globalIndexPathLens eventToOld eventToNew newIndexPath = do
	oldIndexPath <- use $ globals.globalIndexPathLens
	when  (oldIndexPath /= newIndexPath) $ do
		sendEvent oldIndexPath eventToOld
		sendEvent newIndexPath eventToNew
	globals.globalIndexPathLens .= newIndexPath

sendEvent indexPath event = do
	a <- get
	handler <- return $ (getView a)^?(lensAtIndexPath indexPath).viewEventHandler
	case join handler of
		Just h -> put $ h event
		Nothing -> return ()

indexPathAtPoint :: (Float, Float) -> View a -> IndexPath
indexPathAtPoint point view = fst $ indexPathAtPoint' [] point view

indexPathAtPoint' :: IndexPath -> (Float, Float) -> View a -> (IndexPath, Bool)
indexPathAtPoint' currentPath point@(x,y) view = 
	if eventInView 
		then case map fst . filter snd $ subviewHandlers of
			[] -> (currentPath, True)
			hs -> (last hs, True)
		else (currentPath, False)
	where
	eventInView = pointInExtent (view^.viewFrame) point
	subviewHandlers = map (\(i,v) -> indexPathAtPoint' (currentPath++[i]) translatedPoint v) (subviews)
	translatedPoint = (x-(fromIntegral xmin), y-(fromIntegral ymin))
	subviews = sortBy (comparing (^._2.viewZIndex)) $ zip [0..] $ view^.viewSubviews
	(_, ymin, _, xmin) = takeExtent $ view^.viewFrame

-- | Draw a view hierarchy
drawView :: View a -> Picture
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

orderViews :: [View world] -> [View world]
orderViews = sortBy (comparing (^.viewZIndex))

-- | Append subviews.
(<++) :: View a -> [View a] -> View a
(<++) a b = viewSubviews %~ (++b) $ a
infixl 1 <++

instance Monoid (View a) where
	mempty = initView ((0,0),(0,0))
	mappend a b = viewSubviews %~ (++[b]) $ a

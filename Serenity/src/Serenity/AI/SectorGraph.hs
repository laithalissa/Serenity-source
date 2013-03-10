{-# LANGUAGE Arrows #-}

module Serenity.AI.SectorGraph
(	SectorGraph
) where

import Serenity.Model.Common
import Serenity.Model.Sector

import Control.Lens
import Data.Set(Set)
import qualified Data.Set as Set
import Prelude hiding (id, (.))

data NodeID = NodeID Int deriving(Eq, Ord, Show)

data SectorNode = SectorNode
	{	_nodeID :: NodeID
	,	_nodeLocation :: Location
	,	_nodeSpaceLaneNeighbours :: Set NodeID
	,	_nodeNormalNeighbours :: Set NodeID
	}

data SectorGraph = SectorGraph
	{	_sgNextNodeID :: NodeID
	,	_sgNodes :: Set SectorNode
	,	_sgSector :: Sector
	,	_sgSpaceLaneRadius :: Double
	}

makeLenses ''SectorNode
makeLenses ''SectorGraph

instance Enum NodeID where
	toEnum i = NodeID i
	fromEnum (NodeID i) = i

instance Eq SectorNode where
	n1 == n2 = n1^.nodeID == n2^.nodeID

instance Ord SectorNode where
	compare n1 n2 = compare (n1^.sectorID) (n2^.sectorID)	

-- helpers --

distance :: Location -> Location -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2 )

sortTuple (a1, a2) = if a1 <= a2 then (a1, a2) else (a2, a1)

equationOfLine
	:: Location
	-> Location
	-> (Double -> Double)
equationOfLine (sx,sy) (fx,fy) =
	let	m = (fy-sy) / (fx-sx)
		c = sy - (m * sx)
		y x = m*x + c	
	in 	y

isPointOnLine 
	:: Double -- ^ distance allowed from the line
	-> Location -- ^ point
	-> (Location, Location) -- ^ line: start and end poin
	-> Bool -- ^ true if is on line
isPointOnLine allowedDistance point ((sx,sy), (fx,fy)) =
	let	y = equationOfLine (sx,sy) (fx,fy)
		
		xs = let (sx',fx')=sortTuple (sx,fx) in [sx' .. fx']
		ys = map y xs
		linePoints = zip xs ys
	in	or $ map ((<=allowedDistance) . distance point) linesPoints


isLineOnLine 
	:: Double -- ^ distance allowed from the line
	-> (Location, Location)
	-> (Location, Location)
	-> Bool
isLineOnLine allowedDistance (s1@(x1,y1), f1@(x2,y2)) (s2, f2) =
	let	y1 = equationOfLine s1 f1
		xs = let (x1',x2')=sortTuple (x1,x2) in [ x1' .. x2' ]
		ys = map y1 xs
		points = zip xs ys
		f p = isPointOnLine allowedDistance p (s2, f2)
	in	and $ map f points


lookupNode :: SectorGraph -> NodeID -> SectorNode
lookupNode graph nID = graph<^>nID

(<^>) :: SectorGraph -> NodeID -> SectorNode
graph <^> nID = (!!0) $ filter ((==nID) . _nodeID) $ Set.toList $ graph^.sgNodes

-- | looks up normal neighbours
(|>|) :: SectorGraph -> NodeID -> Set NodeID
graph |>| nID = (graph<^>nID)^.nodeNormalNeighbours

-- | looks up space lanes
(|>>|) :: SectorGraph -> NodeID -> Set NodeID
graph |>>| nID = (graph<^>nID)^.nodeSpaceLaneNeighbours

getEdges
	:: (SectorGraph -> NodeID -> Set NodeID) 
	-> SectorGraph 
	-> Set (NodeID, NodeID)
getEdges g graph = 
	let 	f (seen,edges) nodeID = 
			if Set.member nodeID seen
			then (seen, edges)
			else (Set.insert nodeID seen, 
			      Set.union edges (setToEdge nodeID $ g graph nodeID) )
		setToEdge nodeID setNodeIDs = Set.map (\nid->(nodeID,nid)) setNodeIDs
		(_,edges) = Set.foldl f (Set.empty, Set.empty) $ map _nodeID $ graph^.sgNodes
	in	edges


-- | look existing normal edges
(|-|) :: SectorGraph -> Set (NodeID, NodeID)
graph|-| = getEdges (|>|) graph nodeID

-- | lookup space lane edges
(|--|) :: SectorGraph -> Set (NodeID, NodeID)
graph|--| = getEdges (|>>|) graph nodeID

adjacent :: SectorGraph -> NodeID -> Set NodeID
adjacent graph nID = Set.union (graph|>>|nID) (graph|>|nID)

isConnected :: SectorGraph -> NodeID -> NodeID -> Bool
isConnected graph nID1 nID2 =
	let	neighbours = adjacent graph nID1
	in	Set.member nID2 neighbours

-- | strict, calculates instead of querying graph
isJumpLane' :: SectorGraph -> NodeID -> NodeID -> Bool
isJumpLane' graph nID1 nID2 =
	let	node1 = graph<^>nID1
		node2 = graph<^>nID2
		node1Location = node1^.nodeLocation
		node2Location = node2^.nodeLocation
		spaceLaneRadius = graph^.sgSpaceLaneRadius
		line = (node1Location, node2Location)
		

edgeCost' :: SectorGraph -> NodeID -> NodeID -> Double
edgeCost' graph nID1 nID2 = 
	let	node1 = graph<^>nID1
		node2 = graph<^>nID2
		node1Location = node1^.nodeLocation
		node2Location = node2^.nodeLocation
		spaceLaneRadius = graph^.sgSpaceLaneRadius
		

empty :: Sector -> Double -> SectorGraph
empty sector spaceLaneRadius = SectorGraph
	{	_sgNextNodeID = toEnum 0
	,	_sgNodes = Set.empty
	,	_sgSector = sector
	,	_sgSpaceLaneRadius = spaceLaneRadius
	}

addNode :: Location -> SectorGraph -> (SectorGraph, NodeID)
addNode location graph = 
	let	nID = graph^.sgNextNodeID
		node = SectorNode nID location Set.empty Set.empty
		graph' = graph{_sgNodes=Set.insert node (graph^.sgNodes)}
		graph'' = graph'{_sgNextNodeID = succ nID}
	in	(graph'', nID)


addEdge :: NodeID ->NodeID -> SectorGraph -> SectorGraph
addEdge id1 id2 graph =
	if isConnected graph id1 id2
	then graph
	else let	node1 = graph<^>id1
			node2 = graph<^>id2




make 
	:: Sector
	-> Double -- ^ radius of effect for space lanes
	-> Double -- ^ edge break spacing
	-> Double -- ^ spacing of virtual nodes around the graph
	-> [Location] -- ^ addional virtual nodes
	-> SectorGraph
make sector radius edgeBreak virtualNodeSpacing addionalNodeLocations =
	let	


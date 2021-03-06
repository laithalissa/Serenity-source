{-# LANGUAGE Arrows #-}

module Serenity.AI.SectorGraph
(	SectorGraph
,	NodeID
,	(<^>) -- graph <^> nodeID = Node
,	(|@|) -- graph |@| nodeID = Location
,	(|>|) -- graph |>| nodeID = non-space-lane neighbours: Set NodeID
,	(|>>|) -- graph |>>| nodeID = space-lane neighbours: Set NodeID
,	(|>*>|) -- graph |>*>| nodeID = all neighbours: Set NodeID
,	edgeCost -- graph -> NodeID -> NodeID -> EdgeCost 
,	make 
,	isSpaceLane -- graph -> NodeID -> NodeID -> Bool
) where

import Serenity.Model.Common
import Serenity.Model.Sector

import Control.Arrow
import Control.Category

import Text.Printf
import Debug.Trace(trace)
import Text.Show.Pretty(ppShow)

import Control.Parallel(par)
import Control.Lens
import Data.Maybe(fromJust, isJust)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Prelude hiding (id, (.))

data NodeID = NodeID Int deriving(Eq, Ord, Show)

data SectorNode = SectorNode
	{	_nodeID :: NodeID
	,	_nodeLocation :: Location
	,	_nodeSpaceLaneNeighbours :: Set NodeID
	,	_nodeNormalNeighbours :: Set NodeID
	} deriving(Show)

data SectorGraph = SectorGraph
	{	_sgNextNodeID :: NodeID
	,	_sgNodes :: Set SectorNode
	,	_sgSector :: Sector
	,	_sgSpaceLaneRadius :: Double
	,	_sgEdgeCache :: Maybe (Set SectorEdge)
	} deriving(Show)

data SectorEdge = SectorEdge 
	{	_edgeNode1 :: NodeID
	,	_edgeNode2 :: NodeID
	,	_edgeTimeCost :: Double
	,	_edgeIsSpaceLane :: Bool
	} deriving(Show)

makeLenses ''SectorNode
makeLenses ''SectorGraph
makeLenses ''SectorEdge

instance Enum NodeID where
	toEnum i = NodeID i
	fromEnum (NodeID i) = i

instance Eq SectorNode where
	n1 == n2 = n1^.nodeID == n2^.nodeID

instance Ord SectorNode where
	compare n1 n2 = compare (n1^.nodeID) (n2^.nodeID)	

instance Eq SectorEdge where
	(SectorEdge e1n1 e1n2 _ _) == (SectorEdge e2n1 e2n2 _ _) = ((e1n1==e2n1)&&(e1n2==e2n2))||((e1n1==e2n2)&&(e1n2==e2n1))

instance Ord SectorEdge where
	compare (SectorEdge e1n1 e1n2 _ _) (SectorEdge e2n1 e2n2 _ _) = if e1n1 == e2n1 then compare e1n2 e2n2 else compare e1n1 e2n1

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
	in	or $ map ((<=allowedDistance) . distance point) linePoints


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


getEdgesF
	:: (SectorGraph -> NodeID -> Set NodeID) 
	-> SectorGraph 
	-> Set (NodeID, NodeID)
getEdgesF f' graph = 
	let 	f (seen,edges) nodeID = 
			if Set.member nodeID seen
			then (seen, edges)
			else 	let	seen' = Set.insert nodeID seen
					edges' = Set.union edges (setToEdge nodeID $ f' graph nodeID)
				in 	(seen', edges') -- trace (printf "edges' %s: %s\nedges'':%s\n" (show $ Set.size edges') (show edges') (show edges'')) 
		setToEdge nodeID setNodeIDs = Set.map (\nid->(nodeID,nid)) setNodeIDs
		(_,edges') = foldl f (Set.empty, Set.empty) $ map _nodeID $ Set.toList $ graph^.sgNodes
		edges'' = Set.filter (\(n1,n2) -> (n1 > n2) && (Set.member (n2,n1) edges') ) edges'
	in	edges'' -- trace (printf "edges are: %s" (show edges'')) 


-- | look existing normal edges
getEdges :: SectorGraph -> Set (NodeID, NodeID)
getEdges graph = getEdgesF (|>|) graph

-- | lookup space lane edges
getSpaceLaneEdges :: SectorGraph -> Set (NodeID, NodeID)
getSpaceLaneEdges graph = getEdgesF (|>>|) graph 

adjacent :: SectorGraph -> NodeID -> Set NodeID
adjacent graph nID = Set.union (graph|>>|nID) (graph|>|nID)

isConnected :: SectorGraph -> NodeID -> NodeID -> Bool
isConnected graph nID1 nID2 =
	let	neighbours = adjacent graph nID1
	in	Set.member nID2 neighbours
		

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
	else 	let	node1 = graph<^>id1
			node2 = graph<^>id2
			spaceLaneCheck = isSpaceLane' graph id1 id2 
			(node1N', node2N') = 	if spaceLaneCheck
						then (Set.insert id2 (graph|>>|id1), Set.insert id1 (graph|>>|id2))
						else (Set.insert id2 (graph|>|id1), Set.insert id1 (graph|>|id2))
			
			(node1',node2') = 	if spaceLaneCheck
						then (node1{_nodeSpaceLaneNeighbours=node1N'}, node2{_nodeSpaceLaneNeighbours=node2N'})
						else (node1{_nodeNormalNeighbours=node1N'}, node2{_nodeNormalNeighbours=node2N'})
			nodes' = Set.insert node1' $ Set.insert node2' (graph^.sgNodes)
		in	graph{_sgNodes=nodes'}
			


addSpaceLaneEdge :: NodeID -> NodeID -> SectorGraph -> SectorGraph
addSpaceLaneEdge nID1 nID2 graph = 
	let	node1 = graph<^>nID1
		node2 = graph<^>nID2
		node1' = node1{_nodeSpaceLaneNeighbours = Set.insert nID2 (node1^.nodeSpaceLaneNeighbours)}
		node2' = node2{_nodeSpaceLaneNeighbours = Set.insert nID1 (node2^.nodeSpaceLaneNeighbours)}
		nodes' = Set.insert node1' $ Set.insert node2' (graph^.sgNodes)
	in	graph{_sgNodes=nodes'}


cacheEdges :: SectorGraph -> SectorGraph
cacheEdges graph = 
	let	f es' isSL = Set.map (\(n1,n2) -> SectorEdge n1 n2 (edgeCost' graph n1 n2) isSL) es'
		normalEdges' = f (getEdges graph) False
		spaceLaneEdges' = f (getSpaceLaneEdges graph) True
		edges' = Set.union normalEdges' spaceLaneEdges'
	in	graph{_sgEdgeCache = Just edges'}

-- | cartesian product of 2 lists
cart :: [a] -> [a] -> [(a,a)]
cart [] [] = []
cart as [] = []
cart [] bs = []
cart (a:as) bs = (cart as bs) ++ (map (\b -> (a,b)) bs)


edgeTupleEq :: SectorEdge -> (NodeID, NodeID) -> Bool
edgeTupleEq edge1 (n1, n2) = edge1 == (SectorEdge n1 n2 0.0 False)

cacheSearch :: SectorGraph -> NodeID -> NodeID -> SectorEdge
cacheSearch graph n1 n2 =
	let	cache = fromJust $ graph^.sgEdgeCache
		results = Set.filter (\e -> not $ edgeTupleEq e (n1,n2)) cache
	in	(Set.toList results) !! 0

---------- API ----------
make 
	:: Sector
	-> Double -- ^ radius of effect for space lanes
	-> Double -- ^ edge break spacing
	-> Double -- ^ spacing of virtual nodes around the graph
	-> [Location] -- ^ addional virtual nodes
	-> ([NodeID], SectorGraph)
make sector radius edgeBreak virtualNodeSpacing addionalNodeLocations =
	let	graph = empty sector radius
		planets = sectorPlanets' sector

		nodeF (graph, nodeIDs) planet = let	(graph', nID) = addNode (planet^.planetLocation) graph 
						in	(graph', Map.insert (planet^.planetID) nID nodeIDs)							
		(graph', planetNodeIDs) = foldl nodeF (graph,Map.empty) $ sectorPlanets' sector

		-- | given list of edges and planet return new list of edges including planets
		edgesF edges planet = let	pID=planet^.planetID
						nID = fromJust $ Map.lookup pID planetNodeIDs
						pIDs = connectedPlanetIDs planet
					in	edges ++ [(nID, fromJust $ Map.lookup pID' planetNodeIDs) | pID' <- pIDs]
		edges = foldl edgesF [] $ sectorPlanets' sector

		edgeF g (nID1, nID2) = addSpaceLaneEdge nID1 nID2 g
		-- | graph'' will now have all planet nodes and sector space lanes
		graph'' = foldl edgeF graph' $ edges

		-- | adds additional location
		(graph''', addionalIDs) = addAddionalLocations graph'' addionalNodeLocations
		

		-- | adds edge nodes

		graph'''' = addEdgeNodes graph'''

		cachedGraph = cacheEdges graph''''

	in	(addionalIDs, cachedGraph)

		where

		addAddionalLocations' :: SectorGraph -> [Location] -> SectorGraph
		addAddionalLocations' graph locations = fst $ addAddionalLocations graph locations

		addAddionalLocations :: SectorGraph -> [Location] -> (SectorGraph, [NodeID])
		addAddionalLocations graph nodeLocations' = 
			let	(graph', addionalIDs) = foldl (\(g,ids) l -> let (g',nid)=addNode l g in (g',ids++[nid])) (graph,[]) nodeLocations'
				pairs = cart (Set.toList $ Set.map _nodeID (graph'^.sgNodes)) addionalIDs
				graph'' = foldl (\g (id1,id2)-> addEdge id1 id2 g) graph' $ filter (\(a,b)-> a /= b) pairs
			in 	(graph'', addionalIDs)

		connectedPlanetIDs :: Planet -> [PlanetID]
		connectedPlanetIDs planet = 
			let 	pid=planet^.planetID 
			in 	map (\(pID1,pID2)->if pid==pID1 then pID2 else pID1) $ filter (\(pID1,pID2)-> pID1==pid || pID2==pid) (sector^.sectorSpaceLanes)

		addEdgeNodes :: SectorGraph -> SectorGraph
		addEdgeNodes graph = 
			let	edges = Set.toList $ Set.union (getSpaceLaneEdges graph) (getEdges graph)
				f g (n1, n2) = addEdgeNode n1 n2 g
			in	foldl f graph edges
		-- | addes regular nodes along the edge between nID1 and nID2
		addEdgeNode :: NodeID -> NodeID -> SectorGraph -> SectorGraph
		addEdgeNode nID1 nID2 graph =
			let	node1 = graph<^>nID1
				node2 = graph<^>nID2
				node1Location@(x1,y1) = node1^.nodeLocation
				node2Location@(x2,y2) = node2^.nodeLocation
				y = equationOfLine node1Location node2Location
				xs = let (x,x')=(min x1 x2, max x1 x2) in [x+edgeBreak,x+2*edgeBreak .. x']
				ys = map y xs
				locations = zip xs ys
			in	addAddionalLocations' graph locations
				


(<^>) :: SectorGraph -> NodeID -> SectorNode
graph <^> nID = (!!0) $ filter ((==nID) . _nodeID) $ Set.toList $ graph^.sgNodes

-- | look up node location
(|@|) :: SectorGraph -> NodeID -> Location
graph |@| nID = (graph<^>nID)^.nodeLocation

-- | looks up normal neighbours
(|>|) :: SectorGraph -> NodeID -> Set NodeID
graph |>| nID = (graph<^>nID)^.nodeNormalNeighbours

-- | looks up space lanes
(|>>|) :: SectorGraph -> NodeID -> Set NodeID
graph |>>| nID = (graph<^>nID)^.nodeSpaceLaneNeighbours

-- | lookup all neighbours
(|>*>|) :: SectorGraph -> NodeID -> Set NodeID
graph |>*>| nID = 
	let	a = (graph|>|nID) 
		b = (graph|>>|nID)
	in	a `par` b `par` (Set.union a b)


-- | calculates cost of edge (cached)
edgeCost :: SectorGraph -> NodeID -> NodeID -> Double
edgeCost graph nID1 nID2 = (cacheSearch graph nID1 nID2)^.edgeTimeCost

-- | calculates cost of edge (no cache)
edgeCost' :: SectorGraph -> NodeID -> NodeID -> Double
edgeCost' graph nID1 nID2 = 
	let	multiplier =	if isSpaceLane' graph nID1 nID2
				then graph^.sgSector.sectorSpaceLaneSpeedMultiplier
				else 1.0
		distance' = distance (graph |@| nID1) (graph |@| nID2)
		cost' = distance' / multiplier
	in	cost' -- trace (printf "cost between node %s and %s is %s\n" (show nID1) (show nID2) (show cost')) cost'


-- | calculates if edge between nodes is space lanes (cached)
isSpaceLane :: SectorGraph -> NodeID -> NodeID -> Bool
isSpaceLane graph nID1 nID2 = (cacheSearch graph nID1 nID2)^.edgeIsSpaceLane


-- | strict, calculates instead of querying graph
isSpaceLane' :: SectorGraph -> NodeID -> NodeID -> Bool
isSpaceLane' graph nID1 nID2 =
	let	node1 = graph<^>nID1
		node2 = graph<^>nID2
		node1Location = node1^.nodeLocation
		node2Location = node2^.nodeLocation
		spaceLaneRadius = graph^.sgSpaceLaneRadius
		line = (node1Location, node2Location)
		spaceLanes = getSpaceLaneEdges graph
		loc' (nid1, nid2) = ((graph<^>nid1)^.nodeLocation, (graph<^>nid2)^.nodeLocation)
		f line' = isLineOnLine spaceLaneRadius line line'
	in	or $ map (f . loc') $ Set.toList spaceLanes
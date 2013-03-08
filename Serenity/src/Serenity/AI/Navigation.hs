{-# LANGUAGE Arrows #-}

module Serenity.AI.Navigation
(	nearestPlanet
)  where

import Serenity.Model.Common
import Serenity.Model.Sector
import Serenity.Model.Wire

import Data.Graph.AStar(aStar)
import Data.Set(Set)
import qualified Data.Set as Set

import Control.Lens
import Prelude hiding (id, (.))


--arr' = arr . uncurry

{-
	a star will run on a graph:
	- nodes will be planets unioned with artificial nodes
	- artificial nodes: 
	  - ship current location (always exists)
	  - goal location (always exists)
	  - intersections of space lanes
	  - intervals of space lanes

	- cost of an edge is the time taken to travel
	- an edge will either be boosted by space lane or not

	- each edge needs to be aware if it uses space lane

	stages:
	- make artificial graph
	- node(nodeID, location, adjacentNodes : Set[nodeID])
	- edgeCost :: nodeID -> nodeID -> Time
	- edgeIsSpaceLane :: nodeID -> nodeID -> Bool

	notes:
	- you don't need to know speed of ship to calculate costs, since speed doesn't the best route, just the time to get their.
	- you DO need to know spaceLaneMultiplier
	- cost of edge is: length / spaceLaneMultiplier
-}

-- | produces the optimal route across sector
-- route :: BaseWire (Sector, Entity Ship, Location) [ShipAction]
-- route = proc (sector, entity, destination) -> do
	

data SectorGraph = SectorGraph 
	{	nextNodeID 	:: Int
	,	nodes 		:: Set SectorNode
	,	edges 		:: Set SectorEdge
	}


data NodeID = NodePlanetID Int | NodeID Int

instance Eq NodeID where
	(NodePlanetID id1) == (NodePlanetID id2) = (id1 == id2)
	(NodeID id1) == (NodeID id2) = (id1 == id2)
	_ == _ = False

instance Ord NodeID where
	compare (NodePlanetID id1) (NodePlanetID id2) = compare id1 id2
	compare (NodeID id1) (NodeID id2) = compare id1 id2
	compare (NodePlanetID id1) (NodeID id2) = LT
	compare (NodeID id1) (NodePlanetID id2) = GT

data SectorNode = SectorNode
	{	sectorNodeID :: NodeID
	,	sectorNodeLocation :: Location
	,	sectorNodeNeighbours :: Set NodeID
	}

instance Eq SectorNode where
	n1 == n2 = (sectorNodeID n1) == (sectorNodeID n2)

instance Ord SectorNode where
	compare SectorNode{sectorNodeID=id1} SectorNode{sectorNodeID=id2} = compare id1 id2

data SectorEdge = SectorEdge
	{	sectorEdgeNode1ID :: NodeID
	,	sectorEdgeNode2ID :: NodeID
	,	sectorEdgeCost :: Double
	,	sectorEdgeIsSpaceLane :: Bool
	}

instance Eq SectorEdge where
	e1 == e2 = (((sectorEdgeNode1ID e1) == (sectorEdgeNode1ID e2)) && ((sectorEdgeNode2ID e1) == (sectorEdgeNode2ID e2))) ||
		   (((sectorEdgeNode1ID e1) == (sectorEdgeNode2ID e2)) && ((sectorEdgeNode2ID e1) == (sectorEdgeNode1ID e2)))

instance Ord SectorEdge where
	compare SectorEdge{sectorEdgeNode1ID=e1n1,sectorEdgeNode2ID=e1n2} 
		SectorEdge{sectorEdgeNode1ID=e2n1,sectorEdgeNode2ID=e2n2} = compare (e1n1, e1n2) (e2n1, e2n2)


graphNode' :: SectorGraph -> NodeID -> SectorNode
graphNode' graph nodeID = 
	let	results = Set.filter (\n -> (sectorNodeID n) == nodeID) (nodes graph)
	in	(Set.toList results) !! 0


addNode :: Sector -> SectorGraph -> SectorNode -> Location -> SectorGraph
addNode sector graph node location =
	let	newNodeID = NodeID $ (nextNodeID graph) + 1
		newNodeNeighbours = Set.fromList [sectorNodeID node]
		newNode = SectorNode newNodeID location newNodeNeighbours
		newEdgeCost = calculateEdgeCost sector (sectorNodeLocation node) location False
		newEdge = SectorEdge (sectorNodeID node) newNodeID newEdgeCost False
		newGraphNextNodeID = (nextNodeID graph) + 1
		newGraphNodes = Set.insert newNode (nodes graph)
		newGraphEdges = Set.insert newEdge (edges graph)
	in	graph{nextNodeID=newGraphNextNodeID, nodes=newGraphNodes, edges=newGraphEdges}

-- | warning: the original edge is not removed, 2 edges are added
splitEdge :: Sector -> SectorGraph -> SectorEdge -> Location -> SectorGraph
splitEdge sector graph edge location =
	let	-- node --
		newNodeID = NodeID (nextNodeID graph)
		neighbour1 = graphNode' graph (sectorEdgeNode1ID edge)
		neighbour2 = graphNode' graph (sectorEdgeNode2ID edge)
		newNodeNeighbours = Set.fromList [sectorNodeID neighbour1, sectorNodeID neighbour2]
		newNode = SectorNode newNodeID location newNodeNeighbours
		isSpaceLane = sectorEdgeIsSpaceLane edge
		-- edge1 --
		newEdge1Cost = calculateEdgeCost sector 
						 (sectorNodeLocation neighbour1) 
						 (sectorNodeLocation newNode) 
						 isSpaceLane
		newEdge1 = SectorEdge (sectorNodeID neighbour1) (sectorNodeID newNode) newEdge1Cost isSpaceLane
		-- edge2
		newEdge2Cost = calculateEdgeCost sector 
						 (sectorNodeLocation newNode) 
						 (sectorNodeLocation neighbour2) 
						 isSpaceLane
		newEdge2 = SectorEdge (sectorNodeID newNode) (sectorNodeID neighbour2) newEdge2Cost isSpaceLane	
		-- graph --
		newNextNodeID = (nextNodeID graph) + 1
		newNodes = Set.insert newNode (nodes graph)
		newEdges = Set.insert newEdge2 $ Set.insert newEdge1 (edges graph)
	in	graph{nextNodeID=newNextNodeID, nodes=newNodes, edges=newEdges}
	

-- | makes a sectorGraph directly from sector, no artificial nodes
makeSectorGraph :: Sector -> SectorGraph
makeSectorGraph sector = 
	let	planets = sectorPlanets' sector
		spaceLanes = map spaceLanef $ sector^.sectorSpaceLanes
		nodes = makeNodes planets
		edges = makeEdges spaceLanes
	in	SectorGraph 0 (Set.fromList nodes) (Set.fromList edges)

	where

	spaceLanef :: (PlanetID, PlanetID) -> (Planet, Planet)
	spaceLanef (p1ID, p2ID) = (sectorPlanet' sector p1ID, sectorPlanet' sector p2ID)
	
	makeNodes = map $ makeNode sector
	makeEdges = map $ makeEdge sector
 

mapA f = proc things -> do
	case things of 
		(x:xs) -> do
			x' <- f -< x
			xs' <- mapA f -< xs
			id -< x' : xs'
		[] -> id -< []


makeNode :: Sector -> Planet -> SectorNode
makeNode sector planet = 
	let 	pID = (planet^.planetID)
		nodeID = NodePlanetID (planet^.planetID)
		nodeLocation = planet^.planetLocation
		planetEdgeIDs  = map (NodePlanetID . f pID) (sector^.sectorSpaceLanes)
	  	nodeNeighbours = Set.fromList planetEdgeIDs
	in 	SectorNode nodeID nodeLocation nodeNeighbours

	where
	f :: PlanetID -> SpaceLane -> PlanetID
	f pID (p1, p2) = if p1 == pID then p2 else p1	


makeEdge :: Sector -> (Planet, Planet) -> SectorEdge
makeEdge sector (planet1, planet2) = 
	let	node1ID = NodePlanetID (planet1^.planetID)
		node2ID = NodePlanetID (planet2^.planetID)
		node1Location = planet1^.planetLocation
		node2Location = planet2^.planetLocation
		cost = calculateEdgeCost sector node1Location node2Location True
	in	SectorEdge node1ID node2ID cost True
	


calculateEdgeCost :: Sector -> Location -> Location -> Bool -> Double
calculateEdgeCost sector start end isSpaceLane =
	let	length = distance start end
		cost = if isSpaceLane then sector^.sectorSpaceLaneSpeedMultiplier else 1.0
	in	(cost * length)


-- | takes a sector and a location and returns the nearest planet and its distance
nearestPlanet :: BaseWire (Sector, Location) (PlanetID, Double) 
nearestPlanet = proc (sector, location) -> do
	planets <- arr sectorPlanets' -< sector
	let f = min' $ planetDistance location
	closestPlanet <- arr $ uncurry foldl1 -< (f, planets)
	id -< (closestPlanet^.planetID, planetDistance location closestPlanet)
	
min' :: (a -> Double) -> a -> a -> a
min' f a1 a2 = if (f a1) <= (f a2) then a1 else a2

planetDistance :: Location -> Planet  -> Double
planetDistance location planet = distance location (planet^.planetLocation)

distance :: Location -> Location -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2 )

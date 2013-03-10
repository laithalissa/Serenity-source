{-# LANGUAGE Arrows #-}

module Serenity.AI.Navigation
(	planRoute
,	distance
)  where

import Serenity.Model.Common
import Serenity.Model.Entity
import Serenity.Model.Sector
import Serenity.Model.Ship
import Serenity.Model.Wire
import Serenity.AI.SectorGraph

import Data.VectorSpace
import Data.Graph.AStar(aStar)
import Data.Maybe(fromJust, isJust)
import Data.Set(Set)
import qualified Data.Set as Set

import Text.Show.Pretty(ppShow)

import Text.Printf
import Debug.Trace(trace)
import Serenity.Debug(trace')

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
	



planRoute 
	:: Sector 
	-> Entity Ship 
	-> Position 
	-> Plan
planRoute sector entity (endLocation, endDirection) =
	let	
		path = route sector entityLocation endLocation
		indexes = [0..((length path)-1)]
	in	map (makeAction path) indexes

	where
	entityLocation = entity^.entityData.shipLocation
	entityDirection = entity^.entityData.shipDirection

	makeAction :: [(Location, Location, Bool)] -> Int -> ShipAction
	makeAction path index 
		| (index == 0) = ActionMove (l1, entityDirection) (l2, makeDirection l1 l2) l3
		| (index+1 == length path) = ActionMove (l1, makeDirection m1 m2) (l2, endDirection) l3
		| True = ActionMove (l1, makeDirection m1 m2) (l2, makeDirection l1 l2) l3
		
		where
			(m1, m2, m3) = path!!(index-1)
			(l1, l2, l3) = path!!index
			(k1, k2, k3) = path!!(index+1)

		
makeDirection :: Location -> Location -> Direction
makeDirection start end = normalized (end - start)
	

route :: Sector -> Location -> Location -> [(Location, Location, Bool)]
route sector start end =
	let	graph = makeSectorGraphOld sector
		(graph', startID) = graphAddConnectedNode graph sector start
		(graph'', endID) = graphAddConnectedNode graph' sector end

		path = aStar' graph'' startID endID

		-- helpers
		nodeLoc index = sectorNodeLocation $ path !! index
		isSpaceLane index = sectorEdgeIsSpaceLane $ graphEdge' graph'' (sectorNodeID $ path !! index) (sectorNodeID $ path !! (index+1))

		route' = [ (nodeLoc index, nodeLoc (index+1), isSpaceLane index)  | index <- [0..((length path)-2)] ]
	in	trace ("route for (" ++ (show start) ++ ", " ++ (show end) ++ ") is : " ++ (ppShow route') ++ "\n\n endroute") route'

		

aStar' :: SectorGraphOld -> NodeID -> NodeID -> [SectorNode]
aStar' graph startID endID =
	let	-- | aStar argument 1
		end = graphNode' graph endID
		start = graphNode' graph startID

		neighbour :: SectorNode -> Set SectorNode
		neighbour node = Set.map (graphNode' graph) $ graphNeighbours' graph (sectorNodeID node)

		weight :: SectorNode -> SectorNode -> Double
		weight node1 node2 = sectorEdgeCost $ graphEdge' graph (sectorNodeID node1) (sectorNodeID node2)

		heuristic :: SectorNode -> Double
		heuristic node = distance (sectorNodeLocation end) (sectorNodeLocation node)

		isGoal :: SectorNode -> Bool
		isGoal node = (sectorNodeID node) == endID

		mPath = aStar neighbour weight heuristic isGoal start

	in	if (isJust mPath) 
		then (start : (fromJust mPath))
		else 
			let msg = printf "a star no path found, start:\n%s\n end:\n%s\n graph:\n%s\n" 
					(ppShow start) 
					(ppShow end)
					(ppShow graph)
			in trace msg (start : (fromJust mPath))


graphNeighbours' :: SectorGraphOld -> NodeID -> Set NodeID
graphNeighbours' graph id = 
	let	f (SectorEdge id1 id2 _ _) = case (id1, id2) of
			(id, o) -> o
			(o, id) -> o
			(_, _) -> id
	in	Set.filter (/=id) $ Set.map f $ edges graph 
		
graphNode' :: SectorGraphOld -> NodeID -> SectorNode
graphNode' graph nodeID = 
	let	results = Set.filter (\n -> (sectorNodeID n) == nodeID) (nodes graph)
	in	(Set.toList results) !! 0

graphNodeLocation' :: SectorGraphOld -> NodeID -> Location
graphNodeLocation' graph nid =  sectorNodeLocation $ graphNode' graph nid

graphEdge' :: SectorGraphOld -> NodeID -> NodeID -> SectorEdge
graphEdge' graph n1ID n2ID = 
	let	f e1@(SectorEdge id1 id2 _ _) e2@(SectorEdge id3 id4 _ _)  = 
			if setPairEqual (n1ID, n2ID) (id1, id2)
			then e1
			else e2
	in	foldl1 f (Set.toList $ edges graph)


-- | warning: the original edge is not removed, 2 edges are added
splitEdge :: Sector -> SectorGraphOld -> NodeID -> NodeID -> Location -> SectorGraphOld
splitEdge sector graph node1ID node2ID location =
	let	node1 = graphNode' graph node1ID
		node2 = graphNode' graph node2ID
		edge = graphEdge' graph node1ID node2ID
		spaceLane = sectorEdgeIsSpaceLane edge
		(graph', nodeID') = graphAddNode graph location Nothing
		graph'' = graphAddEdge graph' sector node1ID nodeID' spaceLane
		graph''' = graphAddEdge graph'' sector nodeID' node2ID spaceLane
	in	graph'''
	

-- | makes a sectorGraph directly from sector, no artificial nodes
makeSectorGraphOld :: Sector -> SectorGraphOld
makeSectorGraphOld sector = 
	let	planets = sectorPlanets' sector
		spaceLanes = sector^.sectorSpaceLanes
		graph = emptyGraph
		planetF g p = fst $ graphAddNode g (p^.planetLocation) (Just $ p^.planetID)
		graph' = foldl planetF graph planets
		laneF g (p1ID, p2ID) = graphAddEdge g sector (NodePlanetID p1ID) (NodePlanetID p2ID) True
		graph'' = foldl laneF graph' spaceLanes
	in	graph''


calculateEdgeCost :: Sector -> Location -> Location -> Bool -> Double
calculateEdgeCost sector start end isSpaceLane =
	let	length = distance start end
		cost = if isSpaceLane then sector^.sectorSpaceLaneSpeedMultiplier else 1.0
	in	(length / cost)


-- | takes a sector and a location and returns the nearest planet and its distance

min' :: (a -> Double) -> a -> a -> a
min' f a1 a2 = if (f a1) <= (f a2) then a1 else a2

nearestNode :: SectorGraphOld -> Location -> NodeID
nearestNode graph location = 
	let 	nodes' = Set.toList $ nodes graph
		distances = map (distance location . sectorNodeLocation) nodes'
		f (n1, d1) (n2, d2) = if d1 <= d2 then (n1,d1) else (n2,d2)
	in	sectorNodeID $ fst $ foldl1 f $ zip nodes' distances

planetDistance :: Location -> Planet  -> Double
planetDistance location planet = distance location (planet^.planetLocation)

distance :: Location -> Location -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2 )

setPairEqual :: (Eq a) => (a, a) -> (a, a) -> Bool
setPairEqual (x1, y1) (x2, y2) = ((x1==x2)&&(y1==y2)) || ((x1==y2)&&(y1==x2))
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
		routePlan = map (makeAction path) indexes
	in	routePlan
		

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
	let	(startID : endID : _, graph) = make sector 1.0 5.0 50.0 [start, end]
		path = aStar' graph startID endID

		-- helpers
		
		nodeLoc index = graph |@| (path!!index)
		isSpaceLaneIndex index = isSpaceLane graph (path!!index) (path!!(index+1))

		route' = [ (nodeLoc index, nodeLoc (index+1), isSpaceLaneIndex index)  | index <- [0..((length path)-2)] ]
	in	route' --trace (printf "start:\n%s\n end:\n%s\n graph:\n%s\n route:\n%s\n"  (show start) (show end) (ppShow graph) (ppShow route')) route' 

		

aStar' :: SectorGraph ->  NodeID -> NodeID -> [NodeID]
aStar' graph startID endID =
	let	-- | aStar argument 1

		neighbour :: NodeID -> Set NodeID
		neighbour nodeID = graph |>*>| nodeID

		weight :: NodeID -> NodeID -> Double
		weight nodeID1 nodeID2 = edgeCost graph nodeID1 nodeID2

		heuristic :: NodeID -> Double
		heuristic nodeID = edgeCost graph nodeID endID

		isGoal :: NodeID -> Bool
		isGoal nodeID = nodeID == endID

		mPath = aStar neighbour weight heuristic isGoal startID

	in	if (isJust mPath) 
		then (startID : (fromJust mPath))
		else 
			let msg = printf "a star no path found, start:\n%s\n end:\n%s\n graph:\n%s\n" 
					(ppShow startID) 
					(ppShow endID)
					(ppShow graph)
			in trace msg (startID : (fromJust mPath))

distance :: Location -> Location -> Double
distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2)^2 + (y1 - y2)^2 )

module Astar
( search
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

--import qualified PMapueue as PMap
import qualified PriorityMap as PMap

import Debug.Trace

-- | the used node type must be orderable (a requirement of the map structure)
-- and equatable (for testing whether goal has been reached)
-- also showable for debugging (can safely be removed)
search :: (Show a, Ord a, Eq a)
       => (a -> Double)             -- ^ heuristic:  a function that approximates future cost to nearest goal
       -> ([a] -> [(a, Double)]) -- ^ successors: a function that provides the next nodes, costs and arcs/etc
       -> a                         -- ^ origin:  node from which to start the search
       -> a                         -- ^ target:  node to aim for
       -> Maybe ([a],Double)        -- ^ returns a path option
search heuristic successors origin target =
  let
    -- | frontier: prioritized queue of nodes (and their past-cost) to be checked
    -- sorted by fcost (which is best known distance from origin plus estimated distance to nearest target)
    frontier     = PMap.singleton (heuristic origin) origin 0
    -- | interior: set of nodes which have alread been checked
    interior     = Set.empty
    -- | predecessors: keeps track of best predecessors
    predecessors = Map.empty

    -- | recursively searches for and expands with target nodes in the frontier
    inner frontier interior predecessors
      -- search has failed when frontier becomes empty
      | PMap.null frontier = Nothing
      -- in case target is reached, trace route from origin
      | current == target = Just $ (path, pathcost)
      -- during each iteration, successors are added to frontier
      | otherwise = inner frontier'' interior' predecessors'
      where
        -- Find and remove node with lowest f-cost (best past distance + future estimate / heuristic)
        ((current, pathcost), frontier') = PMap.pop frontier

        -- best known path to current node
        path = backtrack current predecessors

        -- add current node to closed set
        interior' = Set.insert current interior

        -- filter out interior nodes
        filtered = filter
          (\(node,cost) -> (not $ Set.member node interior) && (checkfrontier pathcost frontier' (node,cost)) )
          $ successors path

        -- push new nodes onto frontier
        frontier'' = foldl
          (\acc (node,cost) -> PMap.insert (pathcost + cost + heuristic node) node (pathcost + cost) acc)
          frontier' filtered

        -- update best predecessors
        predecessors' = foldl
          (\acc (node,_) -> Map.insert node (current) acc)
          predecessors filtered

    checkfrontier pathcost frontier (node,cost) =
      case PMap.lookup node frontier of
        Just bestpastcost -> pathcost + cost < bestpastcost
        Nothing -> True


    -- | traces route from origin to target using predecessors map
    backtrack target predecessors =
      case Map.lookup target predecessors of
        -- recursively build path back to origin
        Just node -> target : backtrack node predecessors
        -- if no predecessor for target exists in map, we have arrived at origin
        Nothing            -> [target]
  in
    inner frontier interior predecessors
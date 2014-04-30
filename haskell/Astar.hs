module Astar
(
  search
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

import qualified PQueue as PQ
import qualified PriorityMap as PMap



search :: (Ord a)
       => a                    -- ^ origin:     node from which to start the search
       -> (a -> Bool)          -- ^ predicate:  should return true for target nodes
       -> (a -> [(a, Double, b)]) -- ^ successors: a function that provides the next nodes, costs and arcs/etc
       -> (a -> Double)        -- ^ heuristic:  a function that approximates future cost to nearest goal
       -> Maybe [b]            -- ^ returns a path option
search origin predicate successors heuristic =
  let
    -- | frontier: prioritized queue of nodes (and their past-cost) to be checked
    -- sorted by fcost (which is best known distance from origin plus estimated distance to nearest target)
    frontier     = PQ.singleton (heuristic origin) (origin, 0)
    -- | interior: set of nodes which have alread been checked
    -- initially empty
    interior     = Set.empty
    -- | predecessors: keeps track of best predecessors
    predecessors = Map.empty

    -- | recursively searches for and expands with target nodes in the frontier
    inner frontier interior predecessors
      -- search has failed when frontier becomes empty
      | PQ.null frontier = Nothing
      -- in case predicate holds, trace route from origin
      | predicate current    = Just $ trace current predecessors
      -- skip nodes that have already been checked
        -- NOTE: NOT NEEDED BECAUSE OF FILTER DURING ITERAITONS
        -- Set.member node interior = inner frontier' interior predecessors
      -- during each iteration, successors are added to frontier
      | otherwise =
        let
          -- add current node to closed set
          interior' = Set.insert current interior

          -- get successors and costs (succ, cost, arc) list
          succs1 = successors current
          -- filter out interior nodes
          succs = filter (\(succ,cost,arc) -> not . flip Set.member interior $ succ) succs1
          -- TODO: filter out those that already have better g-score

          -- push new nodes onto frontier
          frontier'' =
            foldl (\acc (succ,cost,arc) ->
            PQ.push (pastcost + cost + heuristic succ) (succ, pastcost + cost) acc
            ) frontier' succs

          predecessors' = foldl (\acc (node,_,arc) -> Map.insert node (current,arc) acc) predecessors succs
        in inner frontier'' interior' predecessors'
      where
        -- Find and remove node with lowest f-cost (best past distance + future estimate / heuristic)
        ((current, pastcost), frontier') = PQ.pop frontier

    -- | traces route from origin to target using predecessors map
    trace target predecessors =
      case Map.lookup target predecessors of
        -- recursively build path back to origin
        Just (node,x) -> x : trace node predecessors
        -- if no predecessor for target exists in map, we have arrived at origin
        Nothing          -> []
  in
    inner frontier interior predecessors
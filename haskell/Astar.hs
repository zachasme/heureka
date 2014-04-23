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




search :: (Ord a)
       => a                    -- ^ origin:     node from which to start the search
       -> (a -> Bool)          -- ^ predicate:  should return true for target nodes
       -> (a -> [(a, Double)]) -- ^ successors: a function that provides the next nodes and costs
       -> (a -> Double)        -- ^ heuristic:  a function that approximates future cost to nearest goal
       -> Maybe [a]            -- ^ returns a path option
search origin predicate successors heuristic =
  let
    -- | frontier: priority queue of nodes (and their past-cost) to be checked, sorted by fcost
    frontier     = PQ.singleton (heuristic origin) (origin, 0)
    -- | interior: set of nodes which have alread been checked
    interior     = Set.empty
    -- | predecessors: keeps track of best predecessors
    predecessors = Map.empty

    inner frontier interior predecessors
      -- search has failed when frontier becomes empty
      | PQ.null frontier = Nothing
      -- in case predicate holds, trace route from origin
      | predicate node    = Just [node]
      -- skip nodes that have already been checked
      -- NOTE: NOT NEEDED BECAUSE OF FILTER DURING ITERAITONS
      -- Set.member node interior = inner frontier' interior predecessors
      -- during each iteration, successors are added to frontier
      | otherwise =
        let
          s = filter (not . flip Set.member interior . fst)
            $ successors node

          t = PQ.find ((== node) . fst) frontier

          -- push new nodes onto frontier
          frontier'' = foldl (\acc (succ,cost) -> PQ.push (pastcost + cost + heuristic succ) (succ, pastcost + cost) acc) frontier' s
          -- add current node to closed set
          interior' = Set.insert node interior
          predecessors' = predecessors
        in inner frontier'' interior' predecessors'
      where
        -- Find and remove node with lowest f-cost
        ((node, pastcost), frontier') = PQ.pop frontier
  in
    inner frontier interior predecessors
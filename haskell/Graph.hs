module Graph
( successors
) where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


type Arc vertex a  = (vertex, vertex, a)

data Graph vertex a = Graph [vertex] [Arc vertex a] deriving (Show)

successors :: Graph vertex a -> vertex -> [vertex]
successors g v = []






empty = Map.empty
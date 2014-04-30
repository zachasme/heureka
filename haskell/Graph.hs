module Graph
( successors
) where


type Arc vertex a  = (vertex, vertex, a)

data Graph vertex a = Graph [vertex] [Arc vertex a] deriving (Show)

successors :: Graph vertex a -> vertex -> [vertex]
successors g v = []
import Astar


import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Street   	= String
type Coordinate = (Int, Int)

type AdjacencyMap = Map Coordinate [(Coordinate, Street)]
type IncidenceMap = Map Street (Set Coordinate)

type City = (AdjacencyMap, IncidenceMap)





parse :: String -> City
parse text = foldl parseTokens (Map.empty,Map.empty) (map words $ lines text)

parseTokens :: City -> [String] -> City
parseTokens (adjacents, incidence) [x1,y1,road,x2,y2] = (adjacents', incidence')
	where
		origin = (read x1, read y1)
		target = (read x2, read y2)
		x = (target, road)
		successors = case Map.lookup origin adjacents of	
			Just xs -> x:xs
			Nothing -> [x]
		y = Set.fromList [origin, target]
		incidenceval = case Map.lookup road incidence of
			Just xs -> Set.union xs y
			Nothing -> y
		adjacents' = Map.insert origin successors adjacents
		incidence' = Map.insert road incidenceval incidence
parseTokens city _ = city





successors :: Coordinate -> City -> [(Coordinate, Street)]
successors node (a,_) = case Map.lookup node a of
	Just x  -> x
	Nothing -> []



intersection :: Street -> Street -> City -> Coordinate
intersection road1 road2 (_, i) =
	case (Map.lookup road1 i, Map.lookup road2 i) of
		(Just xs, Just ys) -> head $ Set.toList $ Set.intersection xs ys
		_ -> error $ "Roads " ++ road1 ++ " and " ++ road2 ++ " do not cross"



route (a:b:path) city = arc:(route (b:path) city)
	where
		(node,arc) = fromJust
			$ find (\(succ,_) -> succ == b)
			$ successors a city
route _ _ = []





cityfilepath = "../data/citymap.txt"



--SktPedersStraede & Larsbjoernsstraede 35 80
--to the corner of Studiestraede & Larsbjoernsstraede 45 70


-- direct distance between two nodes
distance :: (Int,Int) -> (Int,Int) -> Double
distance (x1,y1) (x2,y2) = sqrt(dx*dx+dy*dy)
	where
		dx = fromIntegral x2 - fromIntegral x1
		dy = fromIntegral y2 - fromIntegral y1
		


findroute :: Coordinate -> Coordinate -> City -> Maybe [Street]
findroute origin target city =
	case Astar.search heuristic successors' [origin] target of
		Nothing -> Nothing
		Just (path, cost) -> Just $ route (reverse path) city
	where
		heuristic x = distance x target
		successors' path = map (\(node,arc) -> (node, distance node $ head path)) $ successors (head path) city



main = do
	cityfile <- readFile cityfilepath
	let city         = parse cityfile
	let origin       = intersection "SktPedersStraede" "Larsbjoernsstraede" city
	let target       = intersection "Studiestraede" "Larsbjoernsstraede" city
	print $ findroute origin target city
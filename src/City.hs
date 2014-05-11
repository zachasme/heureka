module City
( parse
, successors
, intersection
, route
) where

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



route (a:b:path) (adjecents,lol) = arc: (route (b:path) (adjecents,lol) )
	where
		succs = fromJust $ Map.lookup a adjecents
		(node,arc) = fromJust $ find (\(succ,_) -> succ == b) succs
route _ _ = []


--main = print $ [1]
main = print $ test3

test1 = intersection "vej1" "vej2" $ parse "10 10 vej1 20 20\n20 20 vej2 30 30\n10 10 vej3 30 30\n30 30 vej4 10 10"
test2 = parse "10 10 vej1 20 20\n20 20 vej2 30 30\n10 10 vej3 30 30\n30 30 vej4 10 10"
test3 = successors (10, 10) $ parse "10 10 vej1 20 20\n20 20 vej2 30 30\n10 10 vej3 30 30\n30 30 vej4 10 10"

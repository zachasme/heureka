module City
( parse
, successors
) where

import Data.Map (Map)
import qualified Data.Map as Map




parse file = foldl parseTokens Map.empty (map words $ lines file)

type City = Map (Int,Int) [((Int,Int),String)]

parseTokens :: City -> [String] -> City
parseTokens city [x1,y1,road,x2,y2] =
	let
		origin = (read x1, read y1)
		successor = (read x2, read y2)
		x = (successor, road)
		successors = case Map.lookup origin city of
			Just xs -> x:xs
			Nothing -> [x]
	in
		Map.insert origin successors city
parseTokens city _ = city



--main = print $ [1]
main = print $ successors (10,10) $ parse "10 10 vej1 20 20\n20 20 vej2 30 30\n10 10 vej3 30 30"



successors  node t = case Map.lookup node t of
	Just x -> x
	Nothing -> []
import City
import Astar

import Debug.Trace

cityfilepath = "../data/citymap.txt"



--SktPedersStraede & Larsbjoernsstraede 35 80
--to the corner of Studiestraede & Larsbjoernsstraede 45 70


-- direct distance between two nodes
distance :: (Int,Int) -> (Int,Int) -> Double
distance (x1,y1) (x2,y2) = sqrt(dx*dx+dy*dy)
	where
		dx = fromIntegral x2 - fromIntegral x1
		dy = fromIntegral y2 - fromIntegral y1
		


main = do
	cityfile <- readFile cityfilepath
	let city         = City.parse cityfile
	let origin       = City.intersection "SktPedersStraede" "Larsbjoernsstraede" city
	let target       = City.intersection "Studiestraede" "Larsbjoernsstraede" city

	let successors (x:_) = map (\(node,arc) -> (node, distance node x)) $ City.successors x city
	    successors _     = []
	let heuristic x  = distance x target
	let (Just (path, pathcost)) = Astar.search heuristic successors origin target
	print $ ("Path cost: " ++ show pathcost, City.route (reverse path) city)
	--print path
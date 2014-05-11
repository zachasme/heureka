import Astar
import Clauses

import Debug.Trace

datafilepath = "../data/pidgeon.txt"



--SktPedersStraede & Larsbjoernsstraede 35 80
--to the corner of Studiestraede & Larsbjoernsstraede 45 70


-- direct distance between two nodes
distance :: (Int,Int) -> (Int,Int) -> Double
distance (x1,y1) (x2,y2) = sqrt(dx*dx+dy*dy)
	where
		dx = fromIntegral x2 - fromIntegral x1
		dy = fromIntegral y2 - fromIntegral y1



main = do
	datafile <- readFile datafilepath
	let clauses         = Clauses.parse datafile
	--let origin       = City.intersection "SktPedersStraede" "Larsbjoernsstraede" city
	--let target       = City.intersection "Studiestraede" "Larsbjoernsstraede" city
	let origin = (0,0)
	let target = (9,9)
	let successors x = []
	let heuristic x  = 1
	print $ Astar.search heuristic successors origin target
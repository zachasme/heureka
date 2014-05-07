import City
import Astar

cityfilepath = "../data/citymap.txt"



--SktPedersStraede & Larsbjoernsstraede 35 80
--to the corner of Studiestraede & Larsbjoernsstraede 45 70


-- direct distance between two nodes
distance (x1,y1) (x2,y2) = 1


main = do
    cityfile <- readFile cityfilepath
    let city         = City.parse cityfile
    let origin       = (45,70)
    let target       = (35,80)
    let successors x = map (\(node,arc) -> (node, distance node x, arc)) $ City.successors x city
    let heuristic x  = distance x target
    print $ Astar.search origin target successors heuristic
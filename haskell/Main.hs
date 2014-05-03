import City
import Astar

src = "../data/citymap.txt"



--SktPedersStraede & Larsbjoernsstraede
--to the corner of Studiestraede & Larsbjoernsstraede.


main = do
    content <- readFile src
    let city = City.parse content
    let origin = City.intersection "SktPedersStraede" "Larsbjoernsstraede"
    let predicate x = x == origin
    let successors = Graph.successors city
    let heuristic = City.distance city origin
    putStrLn $ Astar.search origin predicate successors heuristic
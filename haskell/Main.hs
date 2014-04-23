import City
import Astar

src = "../data/citymap.txt"


main = do
    content <- readFile src
    let city = City.parse content
    let origin = 0
    let target = 0
    let successors = Graph.successors city
    let heuristic = City.distance city origin
    putStrLn $ Astar.search origin target successors heuristic
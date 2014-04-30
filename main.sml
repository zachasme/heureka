app use ["citygraph.sml", "search/astar.sml"];


val c = CityGraph.fromFile "data/citymap.txt";

(*val origin = CityGraph.crossing c "Vestervoldgade" "SktPedersStraede";
val target = CityGraph.crossing c "Studiestraede" "Larsbjoernsstraede";*)
val origin = (10.0,30.0);
val target = (45.0,70.0);

fun isGoal node     = node = target;
fun successors node = CityGraph.successors c node;
fun heuristic  node = 1.0;

val s = Astar.search origin isGoal successors heuristic;

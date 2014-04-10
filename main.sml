use "citygraph.sml";
use "astar.sml";
use "rbfs.sml";


val cityGraph = Citygraph.fromFile "citymap";

RBFS.search cityGraph

(*Astar.search cityGraph;*)
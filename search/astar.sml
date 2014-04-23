use "search/search.sig";

use "lib/set.sml";
use "lib/pq.sml";
use "lib/map.sml";

(* implements the SEARCH signature *)
structure Astar = struct
(* The algorithm works as follows:
 * initially we have two sets, an open set and a closed set
 * visually the open set is the frontier and the closed set is the interior of the visited areas
 **)

  (* Reconstructs path from origin to node *)
  fun trace predecessors node =
      let val predecessor = Map.lookup predecessors node
      in  case predecessor of
            SOME (node,edge) => edge::trace predecessors node
          | NONE => [node]
      end

  (**
   * f-cost: sum of h-cost and g-cost ("full cost?")
   * g-cost: known distance from origin to node (PAST DISATNCE)
   * h-cost: heuristic - approx. distance from node to goal (FUTURE DISTANCE)
   *
   * origin: node from which to begin search
   * isGoal: a function to test whether a node is the destination
   * neighbours: a function that returns a given node's neighbours
   *             along with the cost of moving to them from given node
   * heuristic: a function calculating the approximate cost of
   *            moving from given node to the destination
   *)
  fun search origin isGoal successors heuristic =
    let
      (* frontier: priority queue of nodes ordered by f-cmaposts *)
      fun inner frontier interior predecessors =
(print "inner iteration\n";
        (* if frontier is empty then search has failed *)
        if PQ.null frontier then NONE
        else
          (* find and delete best node from the open set *)
          let val ((node, pastCost), frontier') = PQ.pop frontier
          in
(print "heh"; print "\n";
            (* if goal node reached then construct the path from predecessors and node *)
            if isGoal node
            then SOME (trace predecessors node)
            (* if node has already been seen then discard it and continue *)
            else if Set.member interior node
            then inner frontier' interior predecessors
            else
              let
                (* add node to the closed set *)
                val interior' = Set.insert interior node
                (* TODO: filter out nodes in interior *)
                val filtered = List.filter
                  (fn (succ, cost, edge) =>
                    (not o Set.member interior) succ)
                  (successors node)
                (* insert into frontier *)
                val frontier'' = List.foldl
                  (fn ((successor, cost, edge), pq) => PQ.insert pq (pastCost + cost + heuristic successor) (successor, pastCost + cost))
                  frontier' filtered
                (* update best predecessors *)
                val predecessors' = List.foldl
                  (fn ((successor, cost, edge), m) => Map.insert m successor (node, "wat"))
                  predecessors filtered
              in
                inner frontier'' interior' predecessors'
              end
)(* end node extract*)
          end
)(*end inner iteration*)

      (* initial frontier is just origin node, f-value is heuristic of origin *)
      val frontier = PQ.singleton (heuristic origin) (origin, 0.0)
      (* initial interior is empty *)
      val interior = Set.empty
      (* initially no came froms *)
      val predecessors = Map.empty
    in
      inner frontier interior predecessors 
    end

end (* structure Astar *)
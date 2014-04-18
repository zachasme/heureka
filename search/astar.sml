use "./search.sig";

use "../lib/set.sml";
use "../lib/pq.sml";
use "../lib/map.sml";

(* implements the SEARCH signature *)
structure Astar :> SEARCH = struct

  fun path cameFrom node = [node]

  (**
   * f-cost: sum of h-cost and g-cost ("full cost?")
   * g-cost: known distance from origin to node
   * h-cost: heuristic - approx. distance from node to goal
   *
   * origin: node from which to begin search
   * isGoal: a function to test whether a node is the destination
   * neighbours: a function that returns a given node's neighbours
   *             along with the cost of moving to them from given node
   * heuristic: a function calculating the approximate cost of
   *            moving from given node to the destination
   *)
  fun search origin isGoal neighboursOf heuristic
  = let
      (* frontier: priority queue of nodes ordered by f-cmaposts *)
      fun aux frontier interior gscores cameFrom
      = (* if frontier is empty then search has failed *)
        if PQ.null frontier then NONE
        else
          (* find and delete best node from the open set *)
          let val (node, frontier') = PQ.pop frontier
          in
            (* if goal node reached then construct the path from cameFrom and node *)
            if isGoal node then SOME (path cameFrom node)
            else
              (* if node has already been seen then discard it and continue *)
              if Set.member interior node
              then aux frontier' interior gscores cameFrom
              else
                let
                  val gscore = Map.lookup gscores node
                  (* add node to the closed set *)
                  val interior' = Set.insert interior node
                  (* TODO: filter out nodes in interior *)
                  val successors = map (fn (node, cost) => (s, gscore + cost, heuristic node)) (neighboursOf node)
                  val frontier'' = frontier' (* insert into frontier *)
                  val gscores' = gscores
                  val cameFrom' = cameFrom
                in
                  aux frontier'' interior' gscores' cameFrom'
                end
          end

      (* initial frontier is just origin node, f-value is heuristic of origin *)
      val frontier = PQ.singleton (heuristic origin) origin
      (* initial interior is empty *)
      val interior = Set.empty
      (* initial gscores contain only origin and gscore is 0 *)
      val gscores  = Map.singleton origin 0
      (* initially no came froms *)
      val cameFrom = Map.empty
    in
      aux frontier interior gscores cameFrom 
    end

end (* structure Astar *)
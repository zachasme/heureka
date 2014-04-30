signature SEARCH = sig
	val search :
		''a (* origin *)
		-> (''a -> bool) (* isGoal *)
		-> (''a -> (''a * int) list) (* neigbour nodes func *)
		-> (''a -> int) (* heuristic func *)
		-> (''a list) option (* path *)
end (* signature SEARCH *)
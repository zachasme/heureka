signature SEARCH = sig
	val search :
		''a (* startNode *)
		-> ''a (* endNode *)
		-> (''a -> (''a * int) list) (* neigbours nodes *)
		-> (''a -> int) (* heuristics *)
		-> (''a list) option
end;
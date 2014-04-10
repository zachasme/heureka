signature GRAPH = sig
	type node
	type edge  = node * node
	type graph = edge list

	val fromFile : string -> edge list

	val heuristic :
	    graph
	    -> node (* node *)
		-> int (* approx distance to goal *)

	val neighbours :
		graph
		-> node (* startNode *)
		-> node list (* returns neighbour nodes *)
end;

structure Citygraph :> GRAPH =
struct
	exception Err

	type node   = int * int
	type edge   = node * node
	type graph  = edge list

	fun line2edge line =
		case String.tokens Char.isSpace line of
				x1::y1::r::x2::y2::nil => [((valOf (Int.fromString x1),valOf (Int.fromString y1)),(valOf (Int.fromString x2), valOf (Int.fromString y2)))]
			| _ => nil;

	fun read instream =
		case TextIO.inputLine instream of 
			  SOME line => (line2edge line) @ read instream 
			| NONE      => []
		before TextIO.closeIn instream;

	fun fromFile path = read(TextIO.openIn path)

	fun heuristic graph node = 1

	fun neighbours graph node = nil
end;



val cph = Citygraph.fromFile "citymap";
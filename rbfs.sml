datatype 'a seq
	= Nil
	| Cons of 'a * (unit -> 'a seq);

fun filter p [] = []
	| filter p (x::xs) =
			if p x then x :: filter p xs else filter p xs;

fun rbfs pred next a =
	let fun aux [] _ = [(1,1)] (* no more nodes to check *)
		    | aux (x::xs) ys
		    		= if pred x then [x]
		  				else let val ys = x::ys (* add x to checked*)
		  								 val xs = filter (fn x => List.all (fn y => x<>y) ys) (xs @ next x)
		  					   in x :: aux xs ys
		  					   end
	in
		aux [a] nil
	end;



 

let
	fun line2edge line =
		case String.tokens Char.isSpace line of
				x1::y1::r::x2::y2::nil => [((valOf (Int.fromString x1),valOf (Int.fromString y1)),(valOf (Int.fromString x2), valOf (Int.fromString y2)))]
			| _ => nil

	fun read instream =
		case TextIO.inputLine instream of 
			  SOME line => (line2edge line) @ read instream 
			| NONE      => []
		before TextIO.closeIn instream

	fun fromFile path = read(TextIO.openIn path);

	val edges = fromFile "citymap"

	fun next node =
		let fun aux (((x,y),n)::ys) = n::aux ys
					| aux _ = nil
		in aux edges end

	fun pred x = x = (45, 70)
		val startNode = (35, 80);

in rbfs pred next startNode (*breadth-first: more interesting*)
end;

(* represents a collection of distinct elements *)
signature PRIORITY_QUEUE =
sig
   type 'a priority_queue

   val empty     : 'a priority_queue 
   val singleton : real -> 'a                      -> 'a priority_queue

   val null      : 'a priority_queue              -> bool

   val insert    : 'a priority_queue -> real -> 'a -> 'a priority_queue 
   val pop       : 'a priority_queue              -> 'a * 'a priority_queue
end (* signature PRIORITY_QUEUE *)


(* implements the SET signature *)
structure PQ :> PRIORITY_QUEUE = struct
   type 'a priority_queue = (real * 'a) list

   val empty         = []
   fun singleton i a = [(i, a)]

   val null          = List.null

   fun insert xs i y = (i, y)::xs
   fun pop ((i, x)::xs) = (x, xs)

end (* structure PriorityQueue *)
(* maps keys to values *)
signature MAP =
sig
   type (''a, 'b) map

   val empty     : (''a, 'b) map
   val singleton : ''a -> 'b      -> (''a, 'b) map

   val null      : (''a, 'b) map        -> bool

   val insert    : (''a, 'b) map -> ''a -> 'b -> (''a, 'b) map 
   val delete    : (''a, 'b) map -> ''a -> (''a, 'b) map

   val lookup    : (''a, 'b) map -> ''a -> 'b option
end (* signature MAP *)


(* implements the MAP signature *)
structure Map :> MAP = struct
   type (''a, 'b) map = (''a * 'b) list
   
   val empty         = nil
   fun singleton a b = [(a, b)]

   val null        = List.null

   fun delete nil _         = nil
     | delete ((a,b)::xs) y = if y = a then xs else (a,b)::delete xs y

   fun insert xs a b = (a,b)::(delete xs a)

   fun lookup nil _         = NONE
     | lookup ((a,b)::xs) y = if y = a then SOME b else lookup xs y
end (* structure Map *)



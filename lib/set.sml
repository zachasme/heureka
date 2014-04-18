(* represents a collection of distinct elements *)
signature SET =
sig
   type ''a set

   val empty     : ''a set 
   val singleton : ''a            -> ''a set

   val null      : ''a set        -> bool
   val member    : ''a set -> ''a -> bool

   val insert    : ''a set -> ''a -> ''a set 
   val delete    : ''a set -> ''a -> ''a set 
end (* signature Set *)


(* implements the SET signature *)
structure Set :> SET = struct
   type ''a set = ''a list
   
   val empty       = []
   fun singleton a = [a]

   val null        = List.null
   fun member xs x = List.exists (fn y => y = x) xs

   fun insert xs y = if member xs y then xs else y::xs

   fun delete nil _     = nil
     | delete (x::xs) y = if x = y then xs else x::delete xs y
end (* structure Set *)



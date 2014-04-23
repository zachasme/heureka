load "Real";

app use ["priority_queue_functor.sml"];

structure RealOrderable : ORDERABLE =
struct
  type t = Real.real
  val compare = Real.compare
end

structure PQ = PriorityQueueFunctor(RealOrderable);


val pq = PQ.empty
val pq = PQ.insert pq "WAT" 1.0
val pq = PQ.insert pq "HEJ" 0.0
val (p, pq1) = PQ.pop pq






signature Graph =
struct
	
end




(* should probably be implemented as adjencency list *)
structure CityGraph =
struct
  type identifier = real * real
  type vertex = identifier * string
  type arc    = identifier * identifier * string
  type path   = arc list

  type city = (vertex, (vertex * arc list) Map.map

  fun line2edge m line =
    case String.tokens Char.isSpace line
     of [x1s,y1s,r,x2s,y2s] =>
          let val [x1,y1,x2,y2] = map (valOf o Real.fromString) [x1s,y1s,x2s,y2s]
              val node = (x1,y1)
              val edge = r
              val succ = (x2,y2)
              val cost = Math.sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))

              val tail = case Map.lookup m node of
                SOME x => nil
              | NONE => nil
          in  Map.insert m node ((succ, cost, edge)::tail)
          end
      | _ => m

  fun read instream =
    case TextIO.inputLine instream of 
      SOME line => line2edge (read instream) line 
    | NONE      => Map.empty
    before TextIO.closeIn instream

  fun fromFile path = (read o TextIO.openIn) path



  (* (node, cost, edge) list *)
  fun successors c y =
    case Map.lookup c y of
      SOME xs => xs
    | NONE    => nil
end;
app use ["priority_queue.sig"];

functor PriorityQueueFunctor (Priority : ORDERABLE) : PRIORITY_QUEUE =
  struct
    structure Priority = Priority

    type    priority = Priority.t
    type 'e t        = ('e * priority) list

    exception Empty

    (*val empty  : priority_queue*)
    val empty = nil

    (*val insert : priority_queue -> 'e element -> priority -> priority_queue*)
    fun insert ((x,a)::pq) y b = (case Priority.compare (b, a) of
                                    LESS => (y,b)::(x,a)::pq
                                  | _    => (x,a)::insert pq y b)
      | insert nil         y b = [(y, b)]
    (*val pop : priority_queue -> ('e element * priority_queue)*)
    fun pop ((e,_)::pq) = (e, pq)
      | pop nil         = raise Empty
  end
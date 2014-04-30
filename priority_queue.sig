use "orderable.sig";

signature PRIORITY_QUEUE =
  sig
  	structure Priority : ORDERABLE

    type 'e t
    type    priority = Priority.t
    exception Empty

    val empty  : 'e t
    val insert : 'e t -> 'e -> priority -> 'e t
    val pop    : 'e t -> ('e * 'e t)
  end;
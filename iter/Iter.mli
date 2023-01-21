
val index_seq : int list -> int list Seq.t

val subvector_seq : int list -> int -> (int * int) list Seq.t

val propagate_collapse : int list -> int -> int list -> (int list * int) Seq.t

val middle_el_id : int -> int -> int

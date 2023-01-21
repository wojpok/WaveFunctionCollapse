
(*                   dims   prec   syms    rots *)
val get_permutator : int -> int -> bool -> bool -> ('a list -> 'a list) list

val app_perm : ('a list -> 'a list) list -> 'a list -> 'a list list

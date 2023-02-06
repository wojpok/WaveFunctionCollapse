module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type collection
  type entropy

  (* creating collection *)
  val empty : collection

  val insert : key -> collection -> collection

  (* transforming collection to shanons entropy *)
  val compress_collection : collection -> entropy

  val get_entropy : entropy -> float
  val get_card : entropy -> int

  (* propagation and observation *)
  val filter : (key -> bool) -> entropy -> entropy
  val collapse : int -> entropy -> key

  val propagation_set : 'a * 'b * ('c * 'd list) list -> ('d -> bool) list
  
  val remove_hat : entropy -> int * float * ((int * key) list)
end

module Make(Key : OrderedType) : S with type key = Key.t



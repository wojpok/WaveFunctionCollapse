module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type collection
  type entropy

  val empty : collection

  val insert : key -> collection -> collection

  val compress_collection : collection -> entropy

  val get_entropy : entropy -> float
  val get_card : entropy -> int

  val filter : (key -> bool) -> entropy -> entropy
  val collapse : int -> entropy -> key

  val propagation_set : 'a * 'b * ('c * 'd list) list -> ('d -> bool) list

  val remove_hat : entropy -> int * float * ((int * key) list)
end

module Make(Key : OrderedType) : S with type key = Key.t



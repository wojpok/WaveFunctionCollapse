module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t

  (* initial entropy -> sequence of all emelents -> stack *)
  val of_seq : float -> key Seq.t -> t
  val pop_random : int -> t -> t * key option
  
  val decrease_key : float -> float -> key -> t -> t
end

module Make(Key : OrderedType) : S with type key = Key.t

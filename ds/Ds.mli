module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  type key

  val empty : t
  val singleton : key -> t
  
  val cardinal : t -> int

  val insert : key -> t -> t
  val pick_random : int -> t -> key option
  val remove : key -> t -> t


  val validate : t -> unit
  val depth : t -> int
end

module Make(Key : OrderedType) : S with type key = Key.t
  
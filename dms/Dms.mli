module type OrderedType = sig
    type t
    val compare : t -> t -> int
end

module type S = sig
    type key
    type t
    
    val empty : t
    val insert : key -> t -> t
    val create : key -> t
    val random : int -> t -> key
end

module Make(Key : OrderedType) : S with type key = Key.t
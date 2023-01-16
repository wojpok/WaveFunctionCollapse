module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(S : OrderedType) = struct

  type t = S.t
  

  type 'a node 

  module M = Map.Make(Float)

end
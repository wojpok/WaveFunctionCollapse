module type Key = sig
  type t
end

module type S = sig
  type t
  type 'a vector = 'a Vector.vector

  type _ listDim = 
  | ListRoot : t listDim
  | ListDim  : 'a listDim -> 'a list listDim

  type _ vecDim = 
  | VecRoot : t vecDim
  | VecDim  : 'a vecDim -> 'a vector vecDim

  type ('a, 'b) dim_descriptor = 'a listDim * 'b vecDim

  val dvector_of_dlist   : ('a, 'b) dim_descriptor -> 'a -> 'b
  val dlist_of_dvector   : ('a, 'b) dim_descriptor -> 'b -> 'a
  val initialize_dvector : ('a, 'b) dim_descriptor -> t -> int list -> 'b

  val msize : ('a, 'b) dim_descriptor -> 'b -> int list
  val dims  : ('a, 'b) dim_descriptor -> int

  val mindex : ('a, 'b) dim_descriptor -> int list -> 'b -> t
  val mset   : ('a, 'b) dim_descriptor -> int list -> t -> 'b -> 'b

  val msub_list_append : ('a, 'b) dim_descriptor -> (int * int) list -> 'b -> t list -> t list
  val msub_list        : ('a, 'b) dim_descriptor -> (int * int) list -> 'b -> t list
end

module Make(Key : Key) : S with type t = Key.t
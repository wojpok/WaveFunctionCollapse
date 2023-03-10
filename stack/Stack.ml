module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t

  val of_seq : float -> key Seq.t -> t
  val pop_random : int -> t -> t * key option
  
  val decrease_key : float -> float -> key -> t -> t
end


module Make(Key : OrderedType) : S with type key = Key.t = struct

  type key = Key.t

  module RS = Ds.Make(Key)

  module L = Map.Make(Float)

  (* Map of entropy levels with density set of elements  *)
  type t = RS.t L.t

  let of_seq : float -> key Seq.t -> t = 
    fun entropy seq ->
      let level = Seq.fold_left begin
        fun acc x -> 
          RS.insert x acc
      end RS.empty seq
      in
      L.singleton entropy level

  let pop_random seed stack =
    match L.min_binding_opt stack with
    | None -> stack, None
    | Some (entropy, v) ->
      let rand_el = RS.pick_random seed v in
      let stack = L.update entropy begin function
      | None -> None
      | Some v -> begin match rand_el with
        | Some rand_el ->
          if RS.cardinal v = 1 then
            None
          else
            Some (RS.remove rand_el v)
        | None -> None
        end
      end stack
      in
      (stack, rand_el)
  

  let decrease_key f t el stack =
    let stack = L.update f begin function
      | None -> failwith "decrease key - no such a level"
      | Some v -> if RS.cardinal v = 1 then
          None
        else
          Some(RS.remove el v)
      end stack in
    let stack = L.update t begin function
      | None -> Some(RS.singleton el)
      | Some v ->
        Some(RS.insert el v)
    end stack in stack

end
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(S : OrderedType) = struct

  type key = S.t
  let comp = S.compare

  module M = Map.Make(S)

  type collection = int M.t
  type entropy = int * float * ((int * key) list)

  let empty = M.empty

  let insert el col =
    M.update el begin 
      function
        | Some v -> Some (v + 1)
        | None -> Some 1 
      end 
    col

  let recalc_entropy (card, _, xs) =
    List.fold_left begin
      fun acc (c, _) ->
        let prob = (float_of_int c) /. (float_of_int card) in
        acc -. (prob *. (Float.log10 prob)) 
    end
    0.
    xs
      
  let compress_collection : collection -> entropy = 
    fun col ->
      let flat = M.bindings col in
      let inv = List.map (fun (a,b) -> (b,a)) flat in
      let sorted = List.sort (fun (x, _) (y, _) -> Int.compare y x) inv in
      let cardinality = List.fold_left (fun acc (c, _) -> acc + c) 0 sorted in
      let entropy = recalc_entropy (cardinality, 0., sorted) in
      cardinality, entropy, sorted
  

  let get_entropy (_, e, _) = e

  let filter : (key -> bool) -> entropy -> entropy = 
    fun pred ent -> ignore pred; ent
end
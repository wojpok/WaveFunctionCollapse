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
    if card = 0 then 
      0.
    else
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
  let get_card (c, _, _) = c

  let filter : (key -> bool) -> entropy -> entropy = 
    fun pred (_, _, ent) ->
      let (cx, xs) = List.fold_right begin fun (cx, x) (ca, a) ->
        if pred x then
          ((cx + ca), (cx, x) :: a)
        else
          (ca, a)
      end ent (0, [])
      in
      (cx, recalc_entropy (cx, 0., xs),xs)

  let collapse seed (c, _, ent) = 
    let seed = seed mod c in
    let rec iter seed xs = 
      match xs with
      | (cs, x) :: xs ->
        if seed < cs then
          x
        else
          iter (seed - cs) xs
      | _ -> failwith "how..."
    in iter seed ent

    let propagation_set (_, _, ent) = 
      match ent with
      | [] -> failwith "propagation empty"
      | (_, xs) :: _ ->
        let empties = List.map (fun _ -> []) xs in
        let rec iter xs acc =
          match xs with
          | [] -> acc
          | (_, x) :: xs ->
          let res = List.map2 (fun x a -> if List.mem x a then a else x :: a) x acc in
          iter xs res
        in
        let sets = iter ent empties in
        List.rev @@ List.map (fun x -> (fun y -> List.mem y x)) sets
end
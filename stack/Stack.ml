module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make(S : OrderedType) = struct

  type key = S.t
  let comp = S.compare

  module RS = Map.Make(Int)

  module L = Map.Make(Float)

  type t = key RS.t L.t

  let init : float -> key list -> t = 
    fun entropy keys ->
    let rec iter n xs acc =
      match xs with
      | [] -> acc
      | x :: xs ->
        RS.add n x acc |> iter (n + 1) xs
    in
    L.add entropy (iter 0 keys RS.empty) L.empty 

  let pop_random stack seed =
    match L.min_binding_opt stack with
    | None -> stack, None
    | Some (entropy, v) ->
      let size = RS.cardinal v in
      let index = seed mod size in
      let el = RS.find index v in
      if size = 0 then
        let stack = L.remove entropy stack in
        stack, Some el
      else if index = size - 1 then
        let stack = L.update entropy begin function
          | None -> None
          | Some v -> Some (RS.remove index v)
        end stack
      in
        stack, Some el
      else 
        let stack = L.update entropy begin function
          | None -> None
          | Some v -> 
            let last = RS.find (size - 1) v in
            let v = RS.remove (size - 1) v in
            let v = RS.add index last v in
            Some(v)
          end stack
        in
          stack, Some el

let 


end
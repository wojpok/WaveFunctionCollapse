
(* all points on n - dimensional map *)
let rec index_seq = function
| [] -> List.to_seq [[]]
| x :: xs ->
  let nats = Seq.init x (fun x -> x) in
  let sub = index_seq xs in
  Seq.flat_map begin fun x ->
    Seq.map begin fun y ->
      x :: y
    end sub
  end nats

(* ids of tile *)
let subvector_seq xs prec = 
  let seq = index_seq (List.map (fun x -> x - (2 * prec)) xs) in
  Seq.map begin fun x -> 
    List.map (fun x -> (x, x + (2 * prec))) x
  end seq

let index_dff dims prec =
  let size = 2 * prec + 1 in
  let size = List.fold_left (fun acc _ -> acc * size) 1 dims in
  Seq.init size (fun x -> size - 1 - x)

let propagate_collapse dims prec xs =
  let rec is_inbounds size xs =
    match size, xs with
    | s :: size, x :: xs ->
      if x < s && x >= 0 then
        is_inbounds size xs
      else
        false
    | [], [] -> true
    | _ -> false
  in
  let rec substract_vector xs ys = 
    match xs, ys with
    | x :: xs, y :: ys -> (x + y - prec) :: substract_vector xs ys
    | [], [] -> []
    | _ -> failwith "substract_vector"
  in
  let size = 2 * prec + 1 in
  let ids = index_seq @@ List.map (fun _ -> size) xs in
  let ids = Seq.map begin fun x ->
    substract_vector xs x
  end ids in
  let ids = Seq.map2 (fun a b -> (a, b)) ids @@ index_dff dims prec in
  Seq.filter (fun (x, _) -> is_inbounds dims x) ids


let middle_el_id dims prec =
  let rec iter dims = 
    if dims = 0 then
      1
    else
      (2 * prec + 1) * (iter (dims - 1))
  in
  ((iter dims) - 1) / 2  




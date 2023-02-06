

type 'a node = 
| Node of int * 'a * 'a node * 'a node
| Empty
and  'a vector = int * 'a node

let vector_of_list xs =
  let rec skip n xs = 
    if n = 0 then xs else skip (n - 1) (List.tl xs)
  in
  let rec recursive_converting from n xs = 
    match n, (n mod 2) with
    | 0, _ -> Empty
    | 1, _ -> Node(from, List.hd xs, Empty, Empty)
    | _, 1 ->
        let nlen = (n - 1) / 2 in
        let skipped = skip nlen xs in
        Node(from + nlen, List.hd skipped, 
          recursive_converting from nlen xs, 
          recursive_converting (from + nlen + 1) nlen (List.tl skipped))
    | _, 0 ->
        let nlen = n / 2 in
        let skipped = skip (nlen - 1) xs in
        Node(from + nlen - 1, List.hd skipped,
          recursive_converting from (nlen - 1) xs,
          recursive_converting (from + nlen) nlen (List.tl skipped))
    | _ -> failwith "Recursive converting: Imposible edge case"
  in
  let len = List.length xs in 
    len, recursive_converting 0 (List.length xs) xs 

let empty size el =
  let rec reduce acc = function
    | 0 -> acc
    | n -> reduce (el :: acc) (n - 1)
  in
  reduce [] size |> vector_of_list 

(* Subvector in continuation passing style *)
let sublist_with_append is it acc (_, vec) = 
  let rec iter vec cont acc = 
    match vec with
    | Empty -> cont acc
    | Node(id, v, l, r) ->
      let lower_bound = is <= id in
      let upper_bound = id <= it in
      match lower_bound, upper_bound with
      | true, true ->
        (iter r (fun acc -> iter l cont (v :: acc)) acc)
      | true, false ->
        iter l cont acc
      | false, true ->
        iter r cont acc
      | false, false -> 
        acc
  in
  iter vec (fun v -> v) acc

let sublist is it vec = 
  sublist_with_append is it [] vec

let list_of_vector (len, vec) =
  sublist_with_append 0 (len - 1) [] (len, vec)
  
let test = vector_of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
let () = ignore test

let find id (_, vec) =
  let rec iter = function
  | Empty -> failwith "Vector::find not found"
  | Node(ic, el, l, r) ->
    if(ic = id)
      then el
      else if ic > id 
        then iter l
        else iter r
  in iter vec

let insert id v (len, vec) =
  let rec iter = function
  | Empty -> failwith "Vector::insert not found"
  | Node(ic, el, l, r) ->
    if(ic = id)
      then Node(ic, v, l, r)
      else if ic > id 
        then Node(ic, el, (iter l), r)
        else Node(ic, el, l, (iter r))
  in (len, iter vec)

let update id trans (len, vec) =
  let rec iter = function
  | Empty -> failwith "Vector::update not found"
  | Node(ic, el, l, r) -> 
    match Int.compare ic id with
    | 0 -> 
      let nv = trans el in
      Node(ic, nv, l, r)
    | -1 -> (* id is greater *)
      let n = iter r in
      Node(ic, el, l, n)
    | _ -> 
      let n = iter l in
      Node(ic, el, n, r)
  in
  let n = iter vec in
  (len, n)

let size (len, _) = len

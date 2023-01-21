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

module Make(Key : OrderedType) : S with type key = Key.t = struct
type key = Key.t
let keycomp = Key.compare

type color = 
| Red
| Black

type node =
| Leaf
| Node of color * key * tree * tree
and tree = int * node

let empty = 0, Leaf

let singleton el = 
  (1, Node(Black, el, empty, empty))

type t = tree

(* Autor balansowania -> https://courses.cs.cornell.edu/cs3110/2021sp/textbook/eff/rb.html *)

let balance = function
| cg, Node(Black, z,          (_, Node(Red, y,          (_, Node(Red, x, (ca, a), (cb, b))), (cc, c))), (cd, d)) 
| cg, Node(Black, z,          (_, Node(Red, x, (ca, a), (_, Node(Red, y, (cb, b), (cc, c)))         )), (cd, d))
| cg, Node(Black, x, (ca, a), (_, Node(Red, z,          (_, Node(Red, y, (cb, b), (cc, c))), (cd, d)))         )
| cg, Node(Black, x, (ca, a), (_, Node(Red, y, (cb, b), (_, Node(Red, z, (cc, c), (cd, d)))         ))         ) ->
  cg, Node(Red, y, ((ca + cb + 1), Node(Black, x, (ca, a), (cb, b))), ((cc + cd + 1), Node(Black, z, (cc, c), (cd, d))))
| n -> n

let insert k t =
  let rec ins key = function
  | _, Leaf -> 1, Node(Red, key, empty, empty)
  | c, Node(color, k, l, r) ->
    if keycomp k key = 1 then
      balance ((c + 1), Node(color, k, ins key l, r))
    else 
      balance ((c + 1), Node(color, k, l, ins key r))
  in
  match ins k t with
  | c, Node(_, k, l, r) -> c, Node(Black, k, l, r)
  | _ -> failwith "insert - imposible error"


let safety = true

let validate t = 
  if not safety then
    ()
  else
    let rec iter = function
    | 0, Leaf -> 0
    | c, Node(_, _, l, r) ->
      let rl = iter l in
      let rr = iter r in 
      if c = rl + rr + 1 then
        c
      else
        failwith "validate - count"
    | _ -> failwith "validate - Leaf"
    in ignore (iter t)

let rec depth = function
| _, Leaf -> 0
| _, Node(_, _, l, r) ->
  1 + (Int.max (depth l) (depth r))

  (*
let rec init n t =
  if n < 0 then
    t
  else
    init (n - 1) @@ insert n t
*)

let cardinal (c, _) = c

let pick_random seed = function
| 0, _ -> None
| c, t ->
  let n = seed mod c in
  let rec iter n = function
  | _, Leaf -> failwith "pick_random"
  | _, Node(_, k, (cl, l), r) ->
    if n = cl then
      Some k
    else if n < cl then
      iter n (cl, l)
    else
      iter (n - cl - 1) r
  in
  iter n (c, t)

let rec flatten n t = 
  if n < 0 then 
    [] 
  else 
    (pick_random n t) :: flatten (n - 1) t


let remove a t = 
  let rec remove_left = function
  | _, Leaf -> failwith "How"
  | _, Node(_, k, (0, Leaf), r) -> (k, r)
  | cc, Node(c, k, l, r) ->
    let (key, l) = remove_left l in
    (key, ((cc - 1), Node(c, k, l, r)))
  in
  let rec iter = function
  | _, Leaf -> failwith "Not found"
  | cc, Node(c, k, l, r) ->
    match keycomp k a with
    |  1 -> ((cc - 1), Node(c, k, iter l, r))
    | -1 -> ((cc - 1), Node(c, k, l, iter r))
    |  0 -> begin match l, r with
      | (_, Leaf), (_, Leaf) -> 0, Leaf
      | (_, Leaf), (_, Node(_,_,_,_) as t)
      | (_, Node(_,_,_,_) as t), (_, Leaf) -> t
      | (l, r) -> 
        let (k, r) = remove_left r in
        ((cc - 1), Node(c, k, l, r))
    end
    | _ -> failwith "remove compare"
  in
  iter t

let tr = ref empty

let rec random_flat c s t = 
  let el = pick_random s t in
  match el with
  | None -> []
  | Some el ->
    let t = remove el t in
    let () = tr := t in
    let () = validate t in
    el :: (random_flat (c + 1) ((s * 2) mod 2000) t)


let rec rmn c s t =
  if c = 0 then 
    t
  else
    let el = pick_random s t in
      match el with
      | None -> t
      | Some el ->
        let () = print_int c;
                  print_endline "==" in
        let t = remove el t in
        let () = validate t in
        rmn (c - 1) (s * 2) t

let () =        
  ignore rmn;
  ignore random_flat;
  ignore flatten

end
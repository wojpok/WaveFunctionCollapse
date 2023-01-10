type key = Int.t
let compare = Int.compare

(* Modified RB tree *)
type colour = 
| Red
| Black

type tree = 
(*        colour   key   count   size of prefix   lt     rt  *)
| Node of colour * key * int   * int            * tree * tree
| Leaf 

type t = int * tree

let get_prefix n = 
  match n with 
  | Leaf -> 0
  | Node(_, _, _, count, _ , _) -> count

let update_prefix n = 
  match n with 
  | Leaf -> Leaf
  | Node(c', k, c, _, l, r) ->
    Node(c', k, c, (get_prefix l) + c, l, r)

let insert k t = 
  let rec iter = function
  | Leaf -> Node(Red, k, 1, 0, Leaf, Leaf)
  | Node(c1, k1, c'1, p2, l1, r1) -> begin
    match compare k k1 with
    | 0 -> Node(c1, k1, c'1 + 1, p2, l1, r1)
    | 1 -> Node(c1, k1, c'1, p2, l1, iter r1)
    | _ -> Node(c1, k1, c'1, p2 + 1, iter l1, r1)
  end
  in let (size, tree) = t in 
  (size + 1, iter tree)

let create k = ignore k; (0, Leaf)

let random s t = 
  let rec iter s = function
  | Leaf -> failwith "random - indexing fucked up"
  | Node(_, k, c, p, l, r) ->
    if(s < p)
      then iter s l
    else if(s >= p + c) 
      then iter (s - p - c) r
    else k
  in let (size, tree) = t in
  iter (s mod size) tree


let balance n =
  match n with
  (* Black, z, Node (Red, y, Node (Red, x, a, b), c), d *)
  | Node(Black, k1, c1, _,  Node (Red, k2, c2, p2, Node (Red, k3, c3, p3, l3, r3), r2), r1)
    -> Node(Red, k2, c2, p2, (Node(Black, k3, c3, p3, l3, r3)), update_prefix (Node(Black, k1, c1, 0, r2, r1)))
  (*| Node(Black, k1, c1, p1, Node (Red, k2, c2, p2, l2, Node (Red, k3, c3, p3, l3, r3)), r1)
    -> failwith "Nie wiem co ja robię, nie wiem kim jestem, zabijcie mnie w końcu"
  *)| n -> n
    (*
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)*)

let rotate_left = function
  | Node(c1, k1, c'1, p1, l1, Node(c2, k2, c'2, p2, l2, r2))
    -> Node(c2, k2, c'2, p1 + c'1 + p2, Node(c1, k1, c'1, p1, l1, l2), r2)
  | _ -> failwith "rotate_left"

let rotate_right = function
  | Node(c1, k1, c'1, p1, Node(c2, k2, c'2, p2, l2, r2), r1)
    -> Node(c2, k2, c'2, p2, l2, Node(c1, k1, c'1, p1 - c'2 - p2, r2, r1))
  | _ -> failwith "rotate_right"

let () = ignore balance; ignore rotate_left; ignore rotate_right ;;


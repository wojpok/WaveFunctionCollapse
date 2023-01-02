module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  
  val add : key -> t -> t
  val create : key -> t
end

module Make(Key : OrderedType) = struct

  type key = Key.t

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

  let balance n =
    match n with
    (* Black, z, Node (Red, y, Node (Red, x, a, b), c), d *)
    | Node(Black, k1, c1, _, Node (Red, k2, c2, p2, Node (Red, k3, c3, p3, l3, r3), r2), r1)
      -> Node(Red, k2, c2, p2, (Node(Black, k3, c3, p3, l3, r3)), update_prefix (Node(Black, k1, c1, 0, r2, r1)))
    | n -> n
      (*
    | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)*)

  let () = ignore balance

  let add k t = ignore k; t
  let create k = ignore k; (0, Leaf)

end
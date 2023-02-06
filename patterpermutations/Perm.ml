(* This is the simplest solution i could this of  *)

let d1p1id : ('a list -> 'a list) list = [
  (* identity *)
  (function
  | [x1; x2; x3] ->
    [x1; x2; x3]
  | _ -> failwith "E");
]

let d1p1rotations : ('a list -> 'a list) list = [
  (* 180 deg *)
  (function
  | [x1; x2; x3] ->
    [x3; x2; x1]
  | _ -> failwith "E");
]

let d1p2id : ('a list -> 'a list) list = [
  (* identity *)
  (function
  | [x1; x2; x3; x4; x5] ->
    [x1; x2; x3; x4; x5]
  | _ -> failwith "E");
]

let d1p2rotations : ('a list -> 'a list) list = [
  (* 180 degs *)
  (function
  | [x1; x2; x3; x4; x5] ->
    [x5; x4; x3; x2; x1]
  | _ -> failwith "E");
]

let d1p2symmetries : ('a list -> 'a list) list = [
  (* weird inversion *)
  (function
  | [x1; x2; x3; x4; x5] ->
    [x2; x1; x3; x5; x4]
  | _ -> failwith "E");
  (function
  | [x1; x2; x3; x4; x5] ->
    [x4; x5; x3; x1; x2]
  | _ -> failwith "E");
]

let d2p1id : ('a list -> 'a list) list = [
  (* identity *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x1; x2; x3; x4; x5; x6; x7; x8; x9]
  | _ -> failwith "E");
]

let d2p1rotations : ('a list -> 'a list) list = [
  (* 90 degs *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x7; x4; x1; x8; x5; x2; x9; x6; x3]
  | _ -> failwith "E");
  (* 180 degs *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x9; x8; x7; x6; x5; x4; x3; x2; x1]
  | _ -> failwith "E");
  (* 270 degs *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x3; x6; x9; x2; x5; x8; x1; x4; x7]
  | _ -> failwith "E");
]

let d2p1symmetries : ('a list -> 'a list) list = [
  (* vert *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x7; x8; x9; x4; x5; x6; x1; x2; x3]
  | _ -> failwith "E");
  (* hor *)
  (function 
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> 
    [x3; x2; x1; x6; x5; x4; x9; x8; x7]
  | _ -> failwith "E");
  (* diag 1 *)
  (function
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
    [x1; x4; x7; x2; x5; x8; x3; x6; x9]
  | _ -> failwith "E");
  (* diag 2 *)
  (function
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
    [x9; x6; x3; x8; x5; x2; x7; x4; x1]
  | _ -> failwith "E");
]

let d2p2id : ('a list -> 'a list) list = [
  (* id *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5] 
     ->
    [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5]
  | _ -> failwith "E")
]

let d2p2rotations : ('a list -> 'a list) list = [
  (* 90 degs *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5]
    -> 
    [e1; d1; c1; b1; a1; 
     e2; d2; c2; b2; a2; 
     e3; d3; c3; b3; a3; 
     e4; d4; c4; b4; a4; 
     e5; d5; c5; b5; a5]
  | _ -> failwith "E");
  (* 180 degs *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5]
    -> 
    [e5; e4; e3; e2; e1; 
     d5; d4; d3; d2; d1; 
     c5; c4; c3; c2; c1; 
     b5; b4; b3; b2; b1; 
     a5; a4; a3; a2; a1]
  | _ -> failwith "E");
  (* 270 degs *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5]
    -> 
    [e1; d1; c1; b1; a1; 
     e2; d2; c2; b2; a2; 
     e3; d3; c3; b3; a3; 
     e4; d4; c4; b4; a4; 
     e5; d5; c5; b5; a5]
  | _ -> failwith "E");
]

let d2p2symmetries : ('a list -> 'a list) list = [
  (* vert *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5] 
     ->
    [a5; a4; a3; a2; a1; 
     b5; b4; b3; b2; b1; 
     c5; c4; c3; c2; c1; 
     d5; d4; d3; d2; d1; 
     e5; e4; e3; e2; e1]
  | _ -> failwith "E");
  (* hor *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5] 
     ->
    [e1; e2; e3; e4; e5; 
     d1; d2; d3; d4; d5; 
     c1; c2; c3; c4; c5; 
     b1; b2; b3; b4; b5; 
     a1; a2; a3; a4; a5]
  | _ -> failwith "E");
  (* diag 1 *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5] 
     ->
    [a1; b1; c1; d1; e1; 
     a2; b2; c2; d2; e2; 
     a3; b3; c3; d3; e3; 
     a4; b4; c4; d4; e4; 
     a5; b5; c5; d5; e5]
  | _ -> failwith "E");
  (* diag 2 *)
  (function
  | [a1; a2; a3; a4; a5; 
     b1; b2; b3; b4; b5; 
     c1; c2; c3; c4; c5; 
     d1; d2; d3; d4; d5; 
     e1; e2; e3; e4; e5] 
     ->
    [e5; d5; c5; b5; a5; 
     e4; d4; c4; b4; a4; 
     e3; d3; c3; b3; a3; 
     e2; d2; c2; b2; a2; 
     e1; d1; c1; b1; a1]
  | _ -> failwith "E");
]

let get_permutator dims precision rotations symmetries =
  match dims, precision, rotations, symmetries with

  (* D1P1 *)
  | 1, 1, false, _ ->
    d1p1id
  | 1, 1, true, _ ->
    d1p1id @ d1p1rotations

  (* D1P2 *)
  | 1, 2, false, false ->
    d1p2id
  | 1, 2, true, false ->
    d1p2id @ d1p2rotations
  | 1, 2, false, true ->
    d1p2id @ d1p2symmetries
  | 1, 2, true, true ->
    d1p2id @ d1p2rotations @ d1p2symmetries

    (* D2P1 *)
  | 2, 1, false, false ->
    d2p1id
  | 2, 1, true, false ->
    d2p1id @ d2p1rotations
  | 2, 1, false, true ->
    d2p1id @ d2p1symmetries
  | 2, 1, true, true ->
    d2p1id @ d2p1rotations @ d2p1symmetries

    (* D2P2 *)
  | 2, 2, false, false ->
    d2p2id
  | 2, 2, true, false ->
    d2p2id @ d2p2rotations
  | 2, 2, false, true ->
    d2p2id @ d2p2symmetries
  | 2, 2, true, true ->
    d2p2id @ d2p2rotations @ d2p2symmetries
  | _ -> [ fun x -> x ]

let app_perm : ('a list -> 'a list) list -> 'a list -> 'a list list = fun fs xs -> 
  List.fold_left (fun acc f -> (f xs) :: acc) [] fs
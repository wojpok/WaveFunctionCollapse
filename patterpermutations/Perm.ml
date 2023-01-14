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
]

let get_permutator dims precision rotations symmetries =
  match dims, precision, rotations, symmetries with
  | 2, 1, false, false ->
    d2p1id
  | 2, 1, true, false ->
    d2p1id @ d2p1rotations
  | 2, 1, false, true ->
    d2p1id @ d2p1symmetries
  | 2, 1, true, true ->
    d2p1id @ d2p1rotations @ d2p1symmetries
  | _ -> [ fun x -> x ]

let app_perm fs xs = 
  List.fold_left (fun acc f -> (f xs) :: acc) [] fs
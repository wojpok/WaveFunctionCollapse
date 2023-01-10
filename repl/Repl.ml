let test_map = [
  [ 0;  1;  2;  3;  4];
  [ 5;  6;  7;  8;  9];
  [10; 11; 12; 13; 14];
  [15; 16; 17; 18; 19];
  [20; 21; 22; 23; 24]
]

let test_vec = Dim.dvector_of_dlist Dim.dim2 test_map 

module S = struct
  type t = int list
  
  let rec compare xs ys = 
    match xs, ys with
    | x :: xs, y :: ys -> begin
      match Int.compare x y with
      | 0 -> compare xs ys
      | n -> n
    end
    | [],     _ :: _ -> -1
    | _ :: _, []     ->  1
    | [],     []     ->  0
end

let dims = Dim.msize Dim.dim2 test_vec

let rec gen xs size =
  let rec iter n xss acc =
    if n < 0 then 
      acc
    else
      let res = List.fold_right
        (fun x acc -> ((n, n + size) :: x) :: acc)
        xss acc 
      in
      iter (n - 1) xss res
  in
  match xs with
  | [] -> [[]]
  | x :: xs ->
    let acc = gen xs size in
    iter (x - 1 - size) acc []
    
let patterns = gen dims 2 |> List.map (fun x -> Dim.msub_list Dim.dim2 x test_vec )

module M = Dms.Make(S)

let collection = List.fold_right (fun x acc -> M.insert x acc) patterns M.empty 

let uncollected = List.map (fun x -> M.random x collection) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
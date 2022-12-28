let test = Vector.vector_of_list [1; 2; 3; 4; 5; 6] |> Vector.list_of_vector

let rec pr = function
  | x :: xs -> print_endline (string_of_int x); pr xs
  | _ -> ()

let () = pr test

let () = print_endline (Dim.string_of_dlist Dim.dim2 [[1; 2; 6; 7; 8; 9; 2;]; [7 ; 4]; [2; 1; 3; 7]])
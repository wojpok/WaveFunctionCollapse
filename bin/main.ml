let test = Vector.vector_of_list [1; 2; 3; 4; 5; 6] |> Vector.list_of_vector

let rec pr = function
  | x :: xs -> print_endline (string_of_int x); pr xs
  | _ -> ()

let () = pr test


let create_config () =
  let rec repl manual precision (rotations, symmetries) = 
    let inp = 
      print_endline "Current config: ";
      print_endline ("Manual: " ^ string_of_bool manual);
      print_endline ("Precision: " ^ string_of_int precision);
      print_endline (("Scanning: rots: ") ^ (string_of_bool rotations) ^ (" syms: ") ^ (string_of_bool symmetries));
      print_string "$ "; flush stdout;
      input_line stdin
    in
    let comms = Str.split (Str.regexp " +") inp in
    match comms with
    | ["done"] -> manual, precision, (rotations, symmetries)
    | "set" :: ("manual" :: (v :: [])) -> begin
      try let v = bool_of_string v in
        repl v precision (rotations, symmetries)
      with _ ->
        print_endline "Expected bool";
        repl manual precision (rotations, symmetries)
      end
    | "set" :: ("precision" :: (v :: [])) -> begin
      try let v = int_of_string v in
        let validate = if v < 1 then 1 else v in
        repl manual validate (rotations, symmetries)
      with _ ->
        print_endline "Expected int";
        repl manual precision (rotations, symmetries)
      end
    | "set" :: ("scanning" :: (v1 :: [v2])) -> begin
      try let v1 = bool_of_string v1 in
        let v2 = bool_of_string v2 in
        repl manual precision (v1, v2)
      with _ ->
        print_endline "Expected int";
        repl manual precision (rotations, symmetries)
      end
    | _ -> 
      print_endline "Invalid command";
      repl manual precision (rotations, symmetries)
  in
  repl false 1 (false, false)


  
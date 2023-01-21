
(*            repl   prec   rots   syms    seed   dimensions *)
type config = bool * int * (bool * bool) * int  * int list

module ConfigMonad = struct
  (*type ('s1, 's2, 's3, 's4, 's5, 'a) t = ('s1 * 's2 * 's3 * 's4 * 's5 -> 'a * 's1 * 's2 * 's3 * 's4 * 's5)*)

  let return x = fun (s1, s2, s3, s4, s5) -> (x, s1, s2, s3, s4, s5)

  let bind m f = fun (s1, s2, s3, s4, s5) ->
    let (x, s1', s2', s3', s4', s5') = m (s1, s2, s3, s4, s5) in f x (s1', s2', s3', s4', s5')

  let run m (s1, s2, s3, s4, s5) = let (_, s1, s2, s3, s4, s5) = m (s1, s2, s3, s4, s5) in (s1, s2, s3, s4, s5)

  let getRepl = fun (s1, s2, s3, s4, s5) -> (s1, s1, s2, s3, s4, s5)
  let getPrec = fun (s1, s2, s3, s4, s5) -> (s2, s1, s2, s3, s4, s5)
  let getScan = fun (s1, s2, s3, s4, s5) -> (s3, s1, s2, s3, s4, s5)
  let getSeed = fun (s1, s2, s3, s4, s5) -> (s4, s1, s2, s3, s4, s5)
  let getDims = fun (s1, s2, s3, s4, s5) -> (s5, s1, s2, s3, s4, s5)

  let putRepl repl = fun (_, s2, s3, s4, s5) -> ((), repl, s2, s3, s4, s5)
  let putPrec prec = fun (s1, _, s3, s4, s5) -> ((), s1, prec, s3, s4, s5)
  let putScan scan = fun (s1, s2, _, s4, s5) -> ((), s1, s2, scan, s4, s5)
  let putSeed seed = fun (s1, s2, s3, _, s5) -> ((), s1, s2, s3, seed, s5)
  let putDims dims = fun (s1, s2, s3, s4, _) -> ((), s1, s2, s3, s4, dims)
end

let (>>=) = ConfigMonad.bind
let (>>>) a b = ConfigMonad.bind a (fun () -> b)

open ConfigMonad

let create_config () = 
  let rec interp () = 
    Printf.printf "\n";
    getRepl >>= fun repl ->         Printf.printf "Repl: %b\n" repl;
    getPrec >>= fun prec ->         Printf.printf "Precision: %d\n" prec;
    getScan >>= fun (rots, syms) -> Printf.printf "Scanning (rots/syms): %b %b\n" rots syms;
    getSeed >>= fun seed ->         Printf.printf "Seed: %d\n" seed;
    getDims >>= fun dims ->         Printf.printf "Dims %s\n" @@ List.fold_right (fun x acc -> (string_of_int x) ^ " " ^ acc) dims "";
    let line = read_line () in
    let args = Str.split (Str.regexp " +") line in
    let() = ignore args in
    match args with
    | ["done"] ->
      return ()
    | ["repl"; v] ->
      putRepl (bool_of_string v) >>> interp ()
    | ["prec"; v] ->
      putPrec (int_of_string v) >>> interp ()
    | ["scan"; rots; syms] ->
      putScan ((bool_of_string rots, bool_of_string syms)) >>> interp ()
    | ["seed"; v] ->
      putSeed (int_of_string v) >>> interp ()
    |  "dims" :: xs ->
      putDims @@ List.map int_of_string xs >>> interp ()
    | _ -> Printf.printf 
      "Usage: 
        done -> save config 
        repl bool -> set repl
        prec int -> set precision
        scan bool bool -> set rotation and symmetries
        seed int -> set seed
        dims int list -> set dims";
      interp ()
  in
  run (interp()) (false, 1, (true, true), 0, [10; 10])

let validate_config (_, prec, _, _, dims) =
  let validate_prec_dims () = 
    match prec, List.length dims with
    | 1, 2
    | 2, 2
    | 2, 1
    -> true
    | _ -> false
  in
  let positive_dims () = 
    List.fold_left (fun acc x -> acc && (x > 0)) true dims
  in
  (positive_dims()) && (validate_prec_dims())
  
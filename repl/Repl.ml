
module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
end

module type WFC = sig
  type key

  val run2 : key list list -> key list list
end

module Make(Key : OrderedStringableType) = struct
  type key = Key.t

  type computation = 
  | InProgress of computation list * 'a vector
  | Done of 'a vector

  let rec repl computation =
    let () = match computation with
    | InProgress(_, _) -> print_endline "Computation in progress"
    | Done(_) -> print_endline "Computation done"

end







module RS : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
  val run : int -> 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let return x s = (x, s) 

  let bind m f s = 
    let (x, s) = m s in
      f x s

  let hash a = 
    let b = (16807 * (a mod 127773)) - 2836 * (a mod 127773) in
    if b > 0 then b else b + 2147483647

  let random = fun x -> (x, hash x) 

  let run s m = let (x, _) = m s in x
end

let (let* ) = RS.bind

let hash a = 
  let b = (16807 * (a mod 127773)) - 2836 * (a mod 127773) in
  if b > 0 then b else b + 2147483647


let rec rand s = Seq.Cons(s, fun () -> rand (hash s))
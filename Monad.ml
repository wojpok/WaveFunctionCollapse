module Alu = struct
  type ('a, 's1, 's2) t = ('s1 * 's2 -> 'a * 's1 * 's2)
  let return x = fun (s1,s2) -> (x, s1, s2)
  let bind m f = fun (s1,s2) -> let (x, s1', s2') = m (s1,s2) in f x (s1', s2')
  let run m (s1,s2) = m (s1,s2)
  let get1 = fun (s1,s2) -> (s1, s1, s2)
  let get2 = fun (s1,s2) -> (s2, s1, s2)
  let put1 s1 = fun (_,s2) -> ((), s1, s2)
  let put2 s2 = fun (s1,_) -> ((), s1, s2)
  let modify1 f = bind get1 (fun x -> put1 (f x))
  let modify2 f = bind get2 (fun x -> put2 (f x))
  let combine f = bind get1 (fun x -> bind get2 (fun y -> return (f x y)))
end

let (let* ) = Alu.bind
let (>>=) a b = Alu.bind a b
let (>>>) a b = Alu.bind a (fun () -> b)
let (let+ ) a b = Alu.bind a (fun () -> b)


let rec copy () =
  let* v = Alu.get1 in
  if v < 0 then
    Alu.return ()
  else
    Alu.modify1 (fun x -> x - 1) >>>
    Alu.modify2 (fun y -> y + 1) >>>
    copy ()



let calc =
  Alu.put1 10 >>>
  Alu.put2 20 >>>
  Alu.combine (fun x y -> x + y) >>= fun v ->
  copy () >>>
  Alu.return v


let res1 = Alu.run calc (0, 0)

 
let calc = 
  let* () = Alu.put1 20 in
  let* () = Alu.put2 10 in
  let* res = Alu.combine (+) in
  let* () = copy () in
  Alu.return res


let res2 = Alu.run calc (0, 0)

let rec repl () =
  let* _ = Alu.put1 @@ int_of_string @@ input_line stdin in
  let* _ = Alu.put2 @@ int_of_string @@ input_line stdin in
  let* v = Alu.combine (+) in
  if v = 0 then
    Alu.return v
  else
    let () = print_endline @@ string_of_int v in
    repl ()
    
let r() = Alu.run (repl()) (0, 0)


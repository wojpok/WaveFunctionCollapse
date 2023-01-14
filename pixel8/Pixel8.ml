
type pixel8 =
| Black
| Blue
| Green
| Cyan
| Red
| Pink
| Yellow
| White

type t = pixel8

let pixel8_code = function
  | Black  -> 0
  | Blue   -> 5
  | Green  -> 2
  | Cyan   -> 6
  | Red    -> 1
  | Pink   -> 4
  | Yellow -> 3
  | White  -> 7

let compare px1 px2 = 
  Int.compare (pixel8_code px1) (pixel8_code px2)

let stringify = function
  | Some pix -> Printf.sprintf "\\e[30;4%dm " (pixel8_code pix)
  | None -> "\\e[30;40m_"

let pixel8_color px = 
  Printf.sprintf "\\e[30;4%dm" (pixel8_code px)


let show_pixel8_map map =
  let rec sparse_line = function
    | x :: xs -> (pixel8_color x) ^ " " ^ (sparse_line xs)
    | _ -> "\\e[30;40m\n"
  in
  let rec sparse_file = function
    | xs :: xss -> (sparse_line xs) ^ (sparse_file xss)
    | _ -> ""
  in
  (* To jest najprostsze rozwiązanie które pozwala mi na żywo podejrzeć kolory *)
  ignore (Sys.command ("echo -e \"" ^ (sparse_file map) ^ "\""))     

let test () = show_pixel8_map [
  [Black; Blue];
  [Blue; Green];
  [Green; Cyan];
  [Cyan; Red];
  [Red; Pink];
  [Pink; Yellow];
  [Yellow; Black];
  [White; Black]
] 
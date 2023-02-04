
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
  | None -> "\\e[37;40mX"

let pixel8_color px = 
  Printf.sprintf "\\e[37;4%dm" (pixel8_code px)

let newline = (pixel8_color Black) ^ "\n"



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

val compare : t -> t -> int

val stringify : t option -> string
val newline : string
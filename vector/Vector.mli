
(* Publicly available vector definition, I have no idea how to hide type and make GADT work *)
type 'a node = 
| Node of int * 'a * 'a node * 'a node
| Empty

type 'a vector = (int * 'a node)

(* Create an empty array of given size and fill with default element *)
val empty : int -> 'a -> 'a vector

(* Builds and maps elements from list *)

(* val build_from_list : 'a list -> ('a -> 'b) -> 'b vector *)

(* Conversion from and to list *)
val vector_of_list : 'a list -> 'a vector
val list_of_vector : 'a vector -> 'a list

(* Some standard operaions *)
val find : int -> 'a vector -> 'a
val insert : int -> 'a -> 'a vector -> 'a vector
val update : int -> ('a -> 'a) -> 'a vector -> 'a vector
val size : 'a vector -> int

(* Special operation - fast sublist calculation *)
val sublist : int -> int -> 'a vector -> 'a list
val sublist_with_append : int -> int -> 'a list -> 'a vector -> 'a list


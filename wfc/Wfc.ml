
type repl = bool


type config = repl * int

module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
end


module PixelList = struct
  type t = Pixel8.t list
  
  let compare = List.compare Pixel8.compare
end

let test_map = let open Pixel8 in [
  []
]


module M = Dms.Make(PixelList)

module Dim = Dim.Make(Pixel8)

let wfc : type a b. (a, b) Dim.dim_descriptor -> config -> a -> a =
  fun desc conf inp ->
    (* Config *)
    let repl, precision = conf in
    (* Conversion to vector *)
    let def_map = Dim.dvector_of_dlist desc inp in
    (* all_subvector boundaries *)
    let sub_boundaries = Dim.get_subvector_ids (Dim.msize desc def_map) precision in
    (* generate state list *)
    let random_state = 
      List.fold_left
        (fun acc el -> 
          let sub = Dim.msub_list desc el def_map in
          M.insert sub acc)
        M.empty
        sub_boundaries
        in
    ignore repl;
    ignore random_state;
    failwith "TODO"
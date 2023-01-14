
(*            repl   prec   rots   syms  *)
type config = bool * int * (bool * bool)

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
	[White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;Black;Black;Black;Black;Black;Black;Black;White;White;White;];
	[White;White;White;White;White;White;Black;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;Black;Black;Black;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;Black;Black;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;Black;White;White;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;Black;Black;Black;Black;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;White;White;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;Black;White;White;White;White;White;White;White;White;Black;White;White;White;];
	[White;White;White;Black;Black;Black;Black;Black;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;White;White;White;Black;White;White;White;White;Black;White;White;White;];
	[White;White;White;White;Black;Black;White;Black;White;Black;Black;Black;Black;White;White;White;];
	[White;White;White;White;Black;White;White;Black;White;White;White;White;White;White;White;White;];
	[White;White;White;White;Black;Black;Black;Black;White;White;White;White;White;White;White;White;];
	[White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;White;];
]


module M = Dms.Make(PixelList)

module Dim = Dim.Make(Pixel8)

let wfc : type a b. (a, b) Dim.dim_descriptor -> config -> a -> M.t =
  fun desc conf inp ->
    (* dims *)
    let dims = Dim.dims desc in
    (* Config *)
    let repl, precision = conf in
    (* Conversion to vector *)
    let def_map = Dim.dvector_of_dlist desc inp in
    (* all_subvector boundaries *)
    let sub_boundaries = Dim.get_subvector_ids (Dim.msize desc def_map) (precision * 2) in
    (* get all permutations *)
    let perms = Perm.get_permutator dims precision true true in 
    let app_perm xs states = 
      List.fold_left 
        (fun acc el -> M.insert el acc)
        states
        (Perm.app_perm perms xs)
      in
    (* generate state list *)
    let random_state = 
      List.fold_left
        (fun acc el -> 
          let sub = Dim.msub_list desc el def_map in
          app_perm sub acc)
        M.empty
        sub_boundaries
        in
    ignore repl;
    ignore random_state;
    random_state


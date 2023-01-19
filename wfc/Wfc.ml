
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



module Ent = Ent.Make(PixelList)

type pixel_state = 
| Collapsed of Pixel8.t
| Unobserved of Ent.entropy

module PixelState = struct
  type t = pixel_state
end

module Grid = Dim.Make(PixelState)

module Dim = Dim.Make(Pixel8)

module Index = struct
  type t = int list
  let compare = List.compare Int.compare
end

module Stack = Stack.Make(Index)

let wfc : type a b c d. (a, b) Dim.dim_descriptor -> (c, d) Grid.dim_descriptor -> config -> a -> Ent.collection =
  fun desc grid_desc conf inp ->
    (* dims *)
    let dims = Dim.dims desc in
    (* Config *)
    let repl, precision, (rots, syms) = conf in
    (* Conversion to vector *)
    let def_map = Dim.dvector_of_dlist desc inp in
    (* all_subvector boundaries *)
    let sub_boundaries = Iter.subvector_seq (Dim.msize desc def_map) precision  in
    (* get all permutations *)
    let perms = Perm.get_permutator dims precision rots syms in 
    let app_perm xs states = 
      List.fold_left 
        (fun acc el -> Ent.insert el acc)
        states
        (Perm.app_perm perms xs)
      in
    (* generate state list *)
    let random_state = 
      Seq.fold_left
        (fun acc el -> 
          let sub = Dim.msub_list desc el def_map in
          app_perm sub acc)
        Ent.empty
        sub_boundaries
        in
    let entropy = Ent.compress_collection random_state in
    let initial_entropy = Ent.get_entropy entropy in
    let grid_size = [100; 100] in
    let grid = Grid.initialize_dvector grid_desc (Unobserved entropy) grid_size in
    let stack = Stack.of_seq initial_entropy @@ Iter.index_seq grid_size in
    ignore stack;
    ignore grid;
    ignore repl;
    print_endline (string_of_float initial_entropy);
    random_state

let test() = wfc Dim.dim2 Grid.dim2 (false, 1, (true, true)) test_map
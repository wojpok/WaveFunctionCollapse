
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

let wfc : type a b c d. (a, b) Dim.dim_descriptor -> (c, d) Grid.dim_descriptor -> config -> a -> d =
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
    let grid_size = [10; 10] in
    let grid = Grid.initialize_dvector grid_desc (Unobserved entropy) grid_size in
    let stack = Stack.of_seq initial_entropy @@ Iter.index_seq grid_size in
    let random = Rand.rand @@ Rand.hash 0 in
    let rec loop random stack grid = 
      match Seq.uncons random with
      | None -> failwith "rand"
      | Some (randv1, random) -> 
      match Seq.uncons random with
      | None -> failwith "rand"
      | Some (randv2, random) -> 
      let (stack, ind) = Stack.pop_random randv1 stack in
      match ind with
      | None -> grid
      | Some ind ->
      match Grid.mindex grid_desc ind grid with
      | Collapsed _ -> failwith "Observing already observed"
      | Unobserved ent ->
      let observation = Ent.collapse randv2 ent in
      let cell = List.nth observation 4 in (* TODO this should depend on dims *)
      let grid = Grid.mset grid_desc ind (Collapsed cell) grid in
      let neight = Iter.propagate_collapse grid_size precision ind in
      let (grid, stack) = Seq.fold_left begin fun (grid, stack) (inds, offset) ->
        match Grid.mindex grid_desc inds grid with
        | Collapsed _ -> (grid, stack)
        | Unobserved ent -> 
          let nent = Ent.filter (fun x -> cell = (List.nth x offset)) ent in
          let grid = Grid.mset grid_desc inds (Unobserved nent) grid in
          let stack = Stack.decrease_key (Ent.get_entropy ent) (Ent.get_entropy nent) inds stack in
          (grid, stack) 
      end (grid, stack) neight in 
      ignore random;
      ignore stack;
      ignore loop;
      if true then
        loop random stack grid
      else
        grid
    in
    ignore repl;
    loop (fun () -> random) stack grid

let test() = wfc Dim.dim2 Grid.dim2 (false, 1, (true, true)) test_map


let show_partial grid = 
  let xss = Grid.dlist_of_dvector Grid.dim2 grid in
  let str = List.fold_right begin fun xs acc ->
    List.fold_right begin fun x acc ->
      let x = match x with
      | Unobserved _ -> None
      | Collapsed v -> Some v in
      (Pixel8.stringify x) ^ acc 
    end xs "\n" ^ acc
  end xss ""
  in
  ignore (Sys.command ("echo -e \"" ^ str ^ "\""))



(*            repl   prec   rots   syms    seed   dimensions *)
type config = bool * int * (bool * bool) * int  * int list

module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
  val newline : string
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

let show_partial grid = 
  let xss = Grid.dlist_of_dvector Grid.dim2 grid in
  let str = List.fold_right begin fun xs acc ->
    List.fold_right begin fun x acc ->
      let x = match x with
      | Unobserved _ -> None
      | Collapsed v -> Some v in
      (Pixel8.stringify x) ^ acc 
    end xs Pixel8.newline ^ acc
  end xss ""
  in
  ignore (Sys.command ("echo -e \"" ^ str ^ "\""))


let wfc : type a b c d. (a, b) Dim.dim_descriptor -> (c, d) Grid.dim_descriptor -> config -> (d -> unit) -> int -> a -> d =


  fun desc grid_desc conf helper fb inp ->
    (* Monada stanów obliczeń *)
    let module Computation = struct
                  (* seed  grid     stack      backtracking             counter 
      type ('a) t = (int * Grid.t * Stack.t * (Grid.t * Stack.t) list * int -> 'a * int * Grid.t * Stack.t * (Grid.t * Stack.t) list * int)
      *)
      let return x = fun (seed, grid, stack, bt, cnt) -> (x, seed, grid, stack, bt, cnt)
      let bind m f = fun (seed, grid, stack, bt, cnt) -> 
        let (x, seed, grid, stack, bt, cnt) = m (seed, grid, stack, bt, cnt) in 
          f x (seed, grid, stack, bt, cnt)

      let (>>=) = bind
      let (>>>) a b = bind a (fun () -> b)

      let run m (seed, grid, stack, bt, cnt) = m (seed, grid, stack, bt, cnt)

      let random (seed, grid, stack, bt, cnt) = (seed, Rand.hash seed, grid, stack, bt, cnt)

      let getStack   (seed, grid, stack, bt, cnt) = (stack, seed, grid, stack, bt, cnt)
      let putStack stack (seed, grid, _, bt, cnt) = ((),    seed, grid, stack, bt, cnt)

      let popRandom = 
        random >>= fun r ->
        getStack >>= fun stack ->
        let (stack, el) = Stack.pop_random r stack in
        putStack stack >>>
        return el

      let getGrid      (seed, grid, stack, bt, cnt) = (grid,  seed, grid, stack, bt, cnt)
      let putGrid grid (seed, _,    stack, bt, cnt) = ((),    seed, grid, stack, bt, cnt)

      let getFromGrid inds =
        getGrid >>= fun grid ->
        let el = Grid.mindex grid_desc inds grid in
        return el 
      
      let putToGrid inds el =
        getGrid >>= fun grid ->
        putGrid @@ Grid.mset grid_desc inds el grid
      
      let getBt       (seed, grid, stack, bt, cnt) = (bt, seed, grid, stack,    bt, cnt)
      let putBt state (seed, grid, stack,  _, cnt) = ((), seed, grid, stack, state, cnt)

      let getCnt     (seed, grid, stack, bt, cnt) = (cnt, seed, grid, stack, bt, cnt)
      let putCnt cnt (seed, grid, stack, bt, _)   = ((),  seed, grid, stack, bt, cnt)

      let storeState el = 
        getBt >>= fun bt ->
        putBt @@ el :: bt >>>
        getCnt >>= fun c -> 
        putCnt (c - 1)
      
      let backtrack n =
        getBt >>= fun bt ->
        match List.nth_opt bt n with
        | None -> return ()
        | Some(grid, stack) ->
          putStack stack >>>
          putGrid grid 
      
    end in
    let () = ignore Computation.backtrack;
    ignore Computation.storeState;
    ignore Computation.getFromGrid;
    ignore Computation.putToGrid;
    ignore Computation.popRandom;
    ignore Computation.run
    in
    (* dims *)
    let dims = Dim.dims desc in
    let fb = ref fb in
    (* Config *)
    let repl, precision, (rots, syms), seed, grid_size = conf in
    let middle_el_id = Iter.middle_el_id dims precision in
    let () = print_int middle_el_id in
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
    let grid = Grid.initialize_dvector grid_desc (Unobserved entropy) grid_size in
    let stack = Stack.of_seq initial_entropy @@ Iter.index_seq grid_size in
    let random = Rand.rand @@ Rand.hash seed in
    let rec loop random stack grid = 
      let () = fb := (!fb - 1) in
      if !fb = 0 then 
        grid
      else
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
      let e = Ent.get_card ent in
      if e = 0 then
        let () = print_endline "";
                 print_int @@ List.hd ind;
                 print_endline "";
                 print_int @@ List.hd @@ List.tl ind
        in
        grid
      else
      let observation = Ent.collapse randv2 ent in
      let propagation = Ent.propagation_set ent in
      let cell = List.nth observation middle_el_id in (* TODO this should depend on dims *)
      let grid = Grid.mset grid_desc ind (Collapsed cell) grid in
      let neight = Iter.propagate_collapse grid_size precision ind in
      let (grid, stack) = Seq.fold_left begin fun (grid, stack) (inds, offset) ->
        match Grid.mindex grid_desc inds grid with
        | Collapsed _ -> (grid, stack)
        | Unobserved ent -> 
          let pred = List.nth propagation offset in
          let nent = Ent.filter (fun x -> ( (cell = (List.nth x offset)) && (pred (List.nth x middle_el_id)))) ent in
          let grid = Grid.mset grid_desc inds (Unobserved nent) grid in
          let stack = Stack.decrease_key (Ent.get_entropy ent) (Ent.get_entropy nent) inds stack in
          (grid, stack) 
      end (grid, stack) neight in 
      ignore random;
      ignore stack;
      ignore loop;
      if true then
        let () = helper grid in
        loop random stack grid
      else
        grid
    in
    ignore repl;
    loop (fun () -> random) stack grid

let test seed x map = wfc Dim.dim2 Grid.dim2 (false, 1, (true, true), seed, [x; x]) (show_partial) map



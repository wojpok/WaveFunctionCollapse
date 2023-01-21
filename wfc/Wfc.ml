
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


let wfc : type a b c d. (a, b) Dim.dim_descriptor -> (c, d) Grid.dim_descriptor -> config -> (d -> unit)  -> a -> d =


  fun desc grid_desc conf helper inp ->
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

      let run m (seed, grid, stack, bt, cnt) = 
        let (res, _, _, _, _, _) = m (Rand.hash seed, grid, stack, bt, cnt) in res

      let random (seed, grid, stack, bt, cnt) = (seed, Rand.hash seed, grid, stack, bt, cnt)

      let getStack   (seed, grid, stack, bt, cnt) = (stack, seed, grid, stack, bt, cnt)
      let putStack stack (seed, grid, _, bt, cnt) = ((),    seed, grid, stack, bt, cnt)

      let popRandom = 
        random >>= fun r ->
        getStack >>= fun stack ->
        let (stack, el) = Stack.pop_random r stack in
        putStack stack >>>
        return el

      let decreaseKey f t k =
        getStack >>= fun s ->
        putStack @@ Stack.decrease_key f t k s

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

      let storeState =
        getGrid >>= fun grid ->
        getStack >>= fun stack -> 
        getBt >>= fun bt ->
        putBt @@ (grid, stack) :: bt >>>
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
    (* Config *)
    let repl, precision, (rots, syms), seed, grid_size = conf in
    let middle_el_id = Iter.middle_el_id dims precision in
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
    
    let (let* ) = Computation.bind in
    let (>>>) a b = Computation.bind a (fun () -> b) in
    let (>>=)  = Computation.bind in

    let rec loop () = 
      let* el = Computation.popRandom in
      match el with
      | None -> Computation.return grid
      | Some inds ->
      let* cellState = Computation.getFromGrid inds in
      match cellState with
      | Collapsed _ -> loop ()
      | Unobserved ent ->
      let e = Ent.get_card ent in
      if e = 0 then
        let () = print_endline "";
                 print_int @@ List.hd inds;
                 print_endline "";
                 print_int @@ List.hd @@ List.tl inds
        in
        Computation.return grid
      else
      let* randv = Computation.random in

      let observation = Ent.collapse randv ent in
      let propagation = Ent.propagation_set ent in

      let cell = List.nth observation middle_el_id in
      
      Computation.putToGrid inds (Collapsed cell) >>>

      let neight = Iter.propagate_collapse grid_size precision inds in

      let rec iter = function
      | [] -> Computation.return ()
      | (inds, offset) :: xs ->
        Computation.getFromGrid inds >>= fun el ->
        match el with
        | Collapsed _ -> iter xs
        | Unobserved ent -> 
          let pred = List.nth propagation offset in
          let nent = Ent.filter (fun x -> ( (cell = (List.nth x offset)) && (pred (List.nth x middle_el_id)))) ent in

          Computation.putToGrid inds (Unobserved nent) >>>
          Computation.decreaseKey (Ent.get_entropy ent) (Ent.get_entropy nent) inds >>> 
          iter xs
      in
      iter (List.of_seq neight) >>>
      Computation.storeState >>>
      Computation.getGrid >>= fun x ->
      helper x;
      loop ()
    in
    ignore repl;
    Computation.run (loop()) (seed, grid, stack, [], -1)

let test seed x map = wfc Dim.dim2 Grid.dim2 (false, 1, (true, true), seed, [x; x]) (show_partial) map



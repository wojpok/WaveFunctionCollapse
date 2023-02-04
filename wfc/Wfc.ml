
(*            repl   prec   rots   syms    seed   dimensions *)
type config = Conf.config

let default_config = Conf.default
let create_config () = Conf.create_config ()

module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
  val newline : string
end

module type S = sig
  type key

  val wfc1 : config -> key list            -> key option list
  val wfc2 : config -> key list list       -> key option list list
  val wfc3 : config -> key list list list  -> key option list list list

  val show1 : key list -> unit
  val show2 : key list list -> unit
  val show3 : key list list list -> unit

  val show1_opt : key option list -> unit
  val show2_opt : key option list list -> unit
  val show3_opt : key option list list list -> unit
end

module Make(Key : OrderedStringableType) : S with type key = Key.t = struct

type key = Key.t

module KeyList = struct
  type t = Key.t list
  let compare = List.compare Key.compare
end

module Ent = Ent.Make(KeyList)

type cell_state = 
| Collapsed of Key.t
| Unobserved of Ent.entropy

let key_of_cell = function
| Collapsed px -> Some px
| Unobserved _ -> None

module CellState = struct
  type t = cell_state
end

module Grid      = Dim.Make(CellState)
module DimOption = Dim.Make(struct type t = Key.t option end)
module Dim       = Dim.Make(Key)


(* konwerter między modułami *)
let doptlist_of_dgrid : type a b c d. (a, b) DimOption.dim_descriptor -> (c, d) Grid.dim_descriptor -> (Grid.t -> DimOption.t) -> d -> a =
  fun desc grid_desc f grid ->
  let xss : c = Grid.dlist_of_dvector grid_desc grid in
  let rec iter_map : type a b c d. (a, b) DimOption.dim_descriptor -> (c, d) Grid.dim_descriptor -> c -> a =
    fun desc grid_desc xss ->
      match desc, grid_desc with
      | (DimOption.ListRoot, _), (Grid.ListRoot, _) -> f xss
      | (DimOption.ListDim tpa, _), (Grid.ListDim tpc, _) ->
        List.map (iter_map (tpa, DimOption.VecRoot) (tpc, Grid.VecRoot)) xss
      | _ -> failwith "Im so done at this point"
  in
  iter_map desc grid_desc xss


module Index = struct
  type t = int list
  let compare = List.compare Int.compare
end

module Stack = Stack.Make(Index)

let wfc : type a b c d e f. (a, b) Dim.dim_descriptor -> (c, d) Grid.dim_descriptor -> (e, f) DimOption.dim_descriptor -> config -> a -> e =

  fun desc grid_desc optdesc conf inp ->

    (* Monada stanów obliczeń *)
    let module Computation = struct
                  (* seed  grid     stack      backtracking             counter 
      type ('a) t = (int * Grid.t * Stack.t * (Grid.t * Stack.t) list * int -> 'a * int * Grid.t * Stack.t * (Grid.t * Stack.t) list * int)
      *)
      let return x = fun (seed, grid, stack, bt, cnt) -> (x, seed, grid, stack, bt, cnt)
      let bind m f = fun (seed, grid, stack, bt, cnt) -> 
        let (x, seed, grid, stack, bt, cnt) = m (seed, grid, stack, bt, cnt) in 
          f x (seed, grid, stack, bt, cnt)

      let (>>=)   = bind
      let (let* ) = bind

      (* Dodatkowy lukier syntaktyczny na x >>= fun () -> y 
       * Podobno operator >>> jest wykorzystywany przez wzorzec Arrow
       * Ale nie występuje tu żaden konflikt ponieważ nie używam tego wzorca *)
      let (>>>) a b = bind a (fun () -> b)

      let run m (seed, grid, stack, bt, cnt) = 
        let (res, _, _, _, _, _) = m (Rand.hash seed, grid, stack, bt, cnt) in res

      (* losowanie wartości *)
      let random (seed, grid, stack, bt, cnt) = (seed, Rand.hash seed, grid, stack, bt, cnt)
      
      let getStack   (seed, grid, stack, bt, cnt) = (stack, seed, grid, stack, bt, cnt)
      let putStack stack (seed, grid, _, bt, cnt) = ((),    seed, grid, stack, bt, cnt)
      
      (* wyciąganie losowego elemntu ze stosu zgodnie z heurystyką minimalnej entropii *)
      let popRandom = 
        random   >>= fun r ->
        getStack >>= fun stack ->
        let (stack, el) = Stack.pop_random r stack in
        putStack stack >>>
        return el
      
      (* zmniejszanie entropii niezaobserwowanych komórek*)
      let decreaseKey f t k =
        getStack >>= fun s ->
        putStack @@ Stack.decrease_key f t k s

      let getGrid       (seed, grid, stack, bt, cnt) = (grid,  seed, grid, stack, bt, cnt)
      let putGrid grid  (seed, _,    stack, bt, cnt) = ((),    seed, grid, stack, bt, cnt)
      let getGridAsList (seed, grid, stack, bt, cnt) = (doptlist_of_dgrid optdesc grid_desc key_of_cell grid,
                                                               seed, grid, stack, bt, cnt)
      (* indexowanie w siatce *)
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
      
      (* Zapamiętywanie stanu obliczeń *)
      let storeState =
        getGrid  >>= fun grid ->
        getStack >>= fun stack -> 
        getBt    >>= fun bt ->
        putBt @@ (grid, stack) :: bt >>>
        getCnt   >>= fun c -> 
        putCnt (c - 1)
      
      (* przywracanie wscześniejszego stanu obliczeń *)
      let backtrack n =
        getBt >>= fun bt ->
        match List.nth_opt bt n with
        | None -> return ()
        | Some(grid, stack) ->
          putStack stack >>>
          putGrid grid 
      
    end in

    (* pomocnicza funkcja do wyświetlania obecnego stanu obliczeń *)
    let helper xss = 
      let str = DimOption.string_of_dlist optdesc Key.stringify Key.newline xss in
      ignore @@ Sys.command ("echo -e \"" ^ str ^ "\"")
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
    
    let open Computation in
    let rec console () =
      let () = print_string "WFC $ "; flush stdout in
      let line = read_line () in
      let args = Str.split (Str.regexp " +") line in

      match args with
      | ["show"] ->
        getGridAsList >>= fun grid ->
        helper grid;
        console ()
      | ["cont"; c] ->
        let c = int_of_string c in
        putCnt c >>>
        return true
      | ["bt"; c] ->
        let c = int_of_string c in
        backtrack c >>>
        console ()
      | ["done"] ->
        return false
      | _ -> console ()

    and collapse_cell ent inds =
      let* randv = random in

      let observation = Ent.collapse randv ent in
      let propagation = Ent.propagation_set @@ Ent.remove_hat ent in

      let cell = List.nth observation middle_el_id in
      
      putToGrid inds (Collapsed cell) >>>

      let neight = Iter.propagate_collapse grid_size precision inds in

      let rec iter = function
      | [] -> return ()
      | (inds, offset) :: xs ->
        getFromGrid inds >>= fun el ->
        match el with
        | Collapsed _ -> iter xs
        | Unobserved ent -> 
          let pred = List.nth propagation offset in
          ignore pred;
          let nent = Ent.filter (fun x -> ( (cell = (List.nth x offset)) && (pred (List.nth x middle_el_id)))) ent in

          putToGrid inds (Unobserved nent) >>>
          decreaseKey (Ent.get_entropy ent) (Ent.get_entropy nent) inds >>> 
          iter xs
      in
      iter (List.of_seq neight)

    and loop () = 
      getCnt >>= fun cnt ->
        if cnt = 0 then
          console () >>= fun cont ->
            if cont then
              loop ()
            else
              getGridAsList
      else
      let* el = popRandom in
      match el with
      | None -> getGridAsList
      | Some inds ->
      let* cellState = getFromGrid inds in
      match cellState with
      | Collapsed _ -> loop ()
      | Unobserved ent ->
      let e = Ent.get_card ent in

      if e = 0 then begin 
        if not repl then
          getGridAsList >>= fun g -> return g
        else
          backtrack 0 >>>
          console () >>= fun cont ->
          if cont then 
            loop()
          else
            getGridAsList >>= fun g -> return g
      end
      else
      collapse_cell ent inds >>>
      storeState >>>
      if repl then
        getGridAsList >>= fun x -> helper x; 
        loop ()
      else 
        loop ()
    in
    run (loop()) (seed, grid, stack, [grid, stack], -1)


let dim1 =      let open Dim in       (ListDim ListRoot,                    VecDim(VecRoot)) 
let dim2 =      let open Dim in       (ListDim(ListDim(ListRoot)),          VecDim(VecDim(VecRoot))) 
let dim3 =      let open Dim in       (ListDim(ListDim(ListDim(ListRoot))), VecDim(VecDim(VecDim(VecRoot))))

let grid_dim1 = let open Grid in      (ListDim ListRoot,                    VecDim(VecRoot)) 
let grid_dim2 = let open Grid in      (ListDim(ListDim(ListRoot)),          VecDim(VecDim(VecRoot))) 
let grid_dim3 = let open Grid in      (ListDim(ListDim(ListDim(ListRoot))), VecDim(VecDim(VecDim(VecRoot))))

let opt_dim1 =  let open DimOption in (ListDim ListRoot,                    VecDim(VecRoot)) 
let opt_dim2 =  let open DimOption in (ListDim(ListDim(ListRoot)),          VecDim(VecDim(VecRoot))) 
let opt_dim3 =  let open DimOption in (ListDim(ListDim(ListDim(ListRoot))), VecDim(VecDim(VecDim(VecRoot))))


let wfc1 conf map = wfc dim1 grid_dim1 opt_dim1 conf map 
let wfc2 conf map = wfc dim2 grid_dim2 opt_dim2 conf map 
let wfc3 conf map = wfc dim3 grid_dim3 opt_dim3 conf map 

let show_ desc xss = 
  let str = DimOption.string_of_dlist desc Key.stringify Key.newline xss in
  ignore @@ Sys.command ("echo -e \"" ^ str ^ "\"")

let show1_opt xs   = show_ opt_dim1 xs
let show2_opt xss  = show_ opt_dim2 xss
let show3_opt xsss = show_ opt_dim3 xsss

let show_ desc xss = 
  let str = Dim.string_of_dlist desc (fun x -> Key.stringify @@ Some x) Key.newline xss in
  ignore @@ Sys.command ("echo -e \"" ^ str ^ "\"")

let show1 xs   = show_ dim1 xs
let show2 xss  = show_ dim2 xss
let show3 xsss = show_ dim3 xsss

end
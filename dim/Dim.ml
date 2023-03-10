module type Key = sig
  type t
end

module type S = sig
  type t
  type 'a vector = 'a Vector.vector

  type _ listDim = 
  | ListRoot : t listDim
  | ListDim  : 'a listDim -> 'a list listDim

  type _ vecDim = 
  | VecRoot : t vecDim
  | VecDim  : 'a vecDim -> 'a vector vecDim

  type ('a, 'b) dim_descriptor = 'a listDim * 'b vecDim

  val dvector_of_dlist   : ('a, 'b) dim_descriptor -> 'a -> 'b
  val dlist_of_dvector   : ('a, 'b) dim_descriptor -> 'b -> 'a
  val initialize_dvector : ('a, 'b) dim_descriptor -> t -> int list -> 'b

  val msize : ('a, 'b) dim_descriptor -> 'b -> int list
  val dims  : ('a, 'b) dim_descriptor -> int

  val mindex : ('a, 'b) dim_descriptor -> int list -> 'b -> t
  val mset   : ('a, 'b) dim_descriptor -> int list -> t -> 'b -> 'b

  val msub_list_append : ('a, 'b) dim_descriptor -> (int * int) list -> 'b -> t list -> t list
  val msub_list        : ('a, 'b) dim_descriptor -> (int * int) list -> 'b -> t list

  val string_of_dlist : ('a, 'b) dim_descriptor -> (t -> string) -> string -> 'a -> string
end

module Make(Key : Key) : S with type t = Key.t = struct
open Vector

  type t = Key.t
  type 'a vector = 'a Vector.vector

  type _ listDim = 
  | ListRoot : t listDim
  | ListDim  : 'a listDim -> 'a list listDim

  type _ vecDim = 
  | VecRoot : t vecDim
  | VecDim  : 'a vecDim -> 'a vector vecDim

  type ('a, 'b) dim_descriptor = 'a listDim * 'b vecDim

  let rec dvector_of_dlist : type a b. (a, b) dim_descriptor -> a -> b =
    fun desc mxs -> 
      match desc with
      | ListDim tpa, VecDim tpb -> 
        List.map (fun x -> dvector_of_dlist (tpa, tpb) x) mxs |> vector_of_list
      | ListRoot, VecRoot -> mxs
      | _ -> failwith "dvector_of_dlist - invalid descriptor"

  let rec dlist_of_dvector : type a b. (a, b) dim_descriptor -> b -> a =
    fun desc mv ->
      match desc with
      | ListDim tpa, VecDim tpb -> 
        list_of_vector mv |> List.map (fun x -> dlist_of_dvector (tpa, tpb) x)
      | ListRoot, VecRoot -> mv
      | _ -> failwith "dlist_of_dvector - invalid descriptor"
  
  let rec initialize_dvector : type a b. (a, b) dim_descriptor -> t -> int list -> b =
    fun desc elem inds ->
      match desc, inds with
      | (_, VecRoot), _ -> elem
      | (_, VecDim tpb), i :: inds ->
        empty i (initialize_dvector (ListRoot, tpb) elem inds)
      | _ -> failwith "initialize_dvector - invalid dimensions" 
    
  
  let string_of_dlist : type a b. (a, b) dim_descriptor -> (t -> string) -> string -> a -> string =
    fun desc string_of_key newline mxs ->
      let rec iter : type a b. (a, b) dim_descriptor -> a -> string = 
        fun desc mxs ->
          match desc with
          | ListDim ListRoot, _ ->
            let m = List.map (fun x -> string_of_key x) mxs in
            List.fold_right (^) m ""
          | ListDim tpa, _ -> 
            let m = List.map (fun x -> (iter (tpa, VecRoot) x)) mxs in 
            (List.fold_right (fun x y -> x ^ newline ^ y) m "\n")
          | ListRoot, _ -> string_of_key mxs  
    in iter desc mxs


  let rec mindex : type a b. (a, b) dim_descriptor -> int list -> b -> t = 
    fun desc inds vec -> 
      match desc, inds with
      | (_, VecDim tpb), ind :: inds ->
        mindex (ListRoot, tpb) inds (find ind vec)
      | (_, VecRoot), _ -> vec 
      | _ -> failwith "mindex - error"
  
  let rec mset : type a b. (a, b) dim_descriptor -> int list -> t -> b -> b =
    fun desc inds v vec ->
      match desc, inds with
      | (_, VecRoot), _ -> v
      | (_, VecDim tpb), i :: inds ->
        let vec = update i (fun vec -> mset (ListRoot, tpb) inds v vec) vec in
        vec
      | _ -> failwith "mse - error"


  let rec msub_list_append : type a b. (a, b) dim_descriptor -> (int * int) list -> b -> t list -> t list =
    fun desc inds vec acc ->
      match desc with
      | (_, VecDim VecRoot) ->
        let (f, t) = List.hd inds in
        sublist_with_append f t acc vec
      | (_, VecDim tpb) ->
        let (f, t) = List.hd inds in
        let subl = sublist f t vec in
        List.fold_right 
          (fun x acc -> msub_list_append (ListRoot, tpb) (List.tl inds) x acc)
          subl
          acc
      | _ -> failwith "msub_list_append - error"

  let msub_list desc inds vec = msub_list_append desc inds vec []

  let rec msize : type a b. (a, b) dim_descriptor -> b -> int list = 
    fun desc vec -> 
      match desc with
      | (_, VecRoot) -> []
      | (_, VecDim tpb) ->
        (size vec) :: (msize (ListRoot, tpb) (find 0 vec))

  let rec get_subvector_ids xs size =
    let rec iter n xss acc =
      if n < 0 then 
        acc
      else
        let res = List.fold_right
          (fun x acc -> ((n, n + size) :: x) :: acc)
          xss acc 
        in
        iter (n - 1) xss res
    in
    match xs with
    | [] -> [[]]
    | x :: xs ->
      let acc = get_subvector_ids xs size in
      iter (x - 1 - size) acc []

  let () = ignore get_subvector_ids

  let rec dims : type a b. (a, b) dim_descriptor -> int =
    fun desc ->
      match desc with
      | ListDim tpa, VecDim tpb -> 1 + (dims (tpa, tpb))
      | ListRoot, VecRoot -> 0
      | _ -> failwith "dims - invalid descriptor"

end


module type S = sig
  type t
end


module Make(Key : S) = struct
open Vector

  type t = Key.t

  type _ listDim = 
  | ListRoot : t listDim
  | ListDim  : 'a listDim -> 'a list listDim

  type _ vecDim = 
  | VecRoot : t vecDim
  | VecDim  : 'a vecDim -> 'a vector vecDim

  type ('a, 'b) dim_descriptor = 'a listDim * 'b vecDim

  let dim1 = (ListDim(ListRoot)),                   (VecDim(VecRoot))
  let dim2 = (ListDim(ListDim(ListRoot))),          (VecDim(VecDim(VecRoot)))
  let dim3 = (ListDim(ListDim(ListDim(ListRoot)))), (VecDim(VecDim(VecDim(VecRoot))))

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

      (*
  let rec string_of_dlist : type a b. (a, b) dim_descriptor -> a -> string =
    fun desc mxs ->
      match desc with
      | ListDim tpa, _ -> 
        let m = List.map (fun x -> (string_of_dlist (tpa, VecRoot) x)) mxs in 
        "[" ^ (List.fold_right (fun x y -> x ^ "; " ^ y) m "]")
      | ListRoot, _ -> string_of_int mxs  
*)

  let rec mindex : type a b. (a, b) dim_descriptor -> int list -> b -> t = 
    fun desc inds vec -> 
      match desc, inds with
      | (_, VecDim tpb), ind :: inds ->
        mindex (ListRoot, tpb) inds (find ind vec)
      | (_, VecRoot), _ -> vec 
      | _ -> failwith "mindex - error"


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
      | _ -> failwith "msub_list_append - 'All fucked up' ~ Iggy Pop"

  let msub_list desc inds vec = msub_list_append desc inds vec []

  let rec msize : type a b. (a, b) dim_descriptor -> b -> int list = 
    fun desc vec -> 
      match desc with
      | (_, VecRoot) -> []
      | (_, VecDim tpb) ->
        (size vec) :: (msize (ListRoot, tpb) (find 0 vec))

        (*
  let basic_map = dvector_of_dlist dim2 
    [
      [1; 2; 3; 4; 5];
      [6; 7; 8; 9; 0];
      [1; 2; 3; 4; 5];
      [6; 7; 8; 9; 0];
      [1; 2; 3; 4; 5]
    ]
  *)

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
   
end


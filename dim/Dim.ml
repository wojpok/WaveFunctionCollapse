open Vector

type t = int

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

let rec string_of_dlist : type a b. (a, b) dim_descriptor -> a -> string =
  fun desc mxs ->
    match desc with
    | ListDim tpa, _ -> 
      let m = List.map (fun x -> (string_of_dlist (tpa, VecRoot) x)) mxs in 
      "[" ^ (List.fold_right (fun x y -> x ^ "; " ^ y) m "]")
    | ListRoot, _ -> string_of_int mxs  


let rec mindex : type a b. (a, b) dim_descriptor -> int list -> b -> t list = 
  fun desc inds vec -> 
    match desc, inds with
    | (_, VecDim tpb), ind :: inds ->
      mindex (ListRoot, tpb) inds (find ind ) 
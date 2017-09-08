open Flx_btype
open Flx_bid

let var_i_occurs i t =
  let rec f_btype t =
    match t with
    | BTYP_type_var (j,_) when i = j -> raise Not_found
    | _ -> Flx_btype.flat_iter ~f_btype t
 in
   try
     f_btype t;
     false
   with Not_found -> true

let rec vars_in t =
  let vs = ref BidSet.empty in
  let add_var i = vs := BidSet.add i !vs in
  let rec f_btype t =
    match t with
    | BTYP_type_var (i,_) -> add_var i
    | _ -> Flx_btype.flat_iter ~f_btype t
  in
  f_btype t;
  !vs

let var_list_occurs ls t =
  let yes = ref false in
  List.iter (fun i -> yes := !yes || var_i_occurs i t) ls;
  !yes



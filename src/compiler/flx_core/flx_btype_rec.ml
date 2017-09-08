open Flx_btype
open Flx_exceptions

(* top down check for fix point not under sum or pointer *)
let rec check_rec t = match t with
   | BTYP_pointer _
   | BTYP_rref _
   | BTYP_wref _
   | BTYP_sum _
   | BTYP_function _
   | BTYP_effector _
   | BTYP_cfunction _
     -> ()

   | BTYP_fix (i,_)
     -> raise Bad_recursion

   | x -> flat_iter ~f_btype:check_rec x


(* tell if a complete type is recursive *)
let is_recursive_type t = 
  let rec ir j t = 
    match t with
    | BTYP_fix (i,_) when i = j -> raise Not_found (* means yes *)
    | _ ->
      let f_btype t = ir (j-1) t in
      Flx_btype.flat_iter ~f_btype t
  in try ir 0 t; false with Not_found -> true
 


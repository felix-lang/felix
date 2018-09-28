open Flx_btype

(* Checks a type is monomorphic, that is, does not contain
  any type variables. The type MAY contain references to 
  polymorphic nominal types, however the indexes of the
  reference must be monomorphic.

  On failure we bug out with an assert. We also check
  that some kinds of type expression cannot occur.
*)

let check_mono bsym_table sr t =
  if Flx_btype_occurs.var_occurs bsym_table t then begin
    print_endline (" **** Failed to monomorphise type " ^ Flx_print.sbt bsym_table t);
    print_endline (" **** got type " ^ Flx_print.sbt bsym_table t);
    assert false
  end
  ;
  match t with
  | BTYP_none 
  | BTYP_type_apply _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    -> 
    (* print_endline ("[flx_numono:check_mono]: Unexpected type expression" ^ sbt bsym_table t); *)
     ()
  | _ -> ()



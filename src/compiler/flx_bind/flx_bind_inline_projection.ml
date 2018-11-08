open Flx_btype
open Flx_bexpr
open Flx_ast

let bind_inline_projection bsym_table be bt sr f' a' ta a =
 (* ---------------------------------------------------------- *)
  (* special case, constant tuple or array projection given by integer *) 
  (* ---------------------------------------------------------- *)
  try match f' with
  | `EXPR_literal (_, {Flx_literal.felix_type="int"; internal_value=s}) ->
    let n = int_of_string s in
    Flx_dot.handle_constant_projection bsym_table sr a ta n
  | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->
  
  (* ---------------------------------------------------------- *)
  (* special case, integer expression as array projection  *) 
  (* ---------------------------------------------------------- *)
  try 
    let f = try be f' with _ -> raise Flx_dot.OverloadResolutionError in
    let int_t = bt sr (`TYP_name (sr,"int",[])) in
    if snd f = int_t then 
    begin
      match ta with
      | BTYP_array _ ->
        Flx_dot.handle_array_projection bsym_table int_t sr a ta f
      | _ -> raise Flx_dot.OverloadResolutionError
    end
    else raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->  
  
  (* ---------------------------------------------------------- *)
  (* special case, unitsum expression as tuple or array projection  *) 
  (* ---------------------------------------------------------- *)
  try match f' with
  (* a dirty hack .. doesn't check unitsum is right size or type *)
  | `EXPR_typed_case (sr,n,sumt) when (match bt sr sumt with | BTYP_unitsum _ -> true | _ -> false)  ->
    Flx_dot.handle_constant_projection bsym_table sr a ta n
  | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  (* ---------------------------------------------------------- *)
  (* special case, array projection  *) 
  (* ---------------------------------------------------------- *)
  try
    let (bf,tf) as f = 
      try be f' 
      with 
      | Flx_exceptions.SimpleNameNotFound _ as x -> raise x
      | exn -> raise Flx_dot.OverloadResolutionError 
    in
    match tf, ta with
    (* Check for array projection *)
    | ixt1, BTYP_array (t,ixt2) when ixt1 = ixt2 -> (* SHOULD USE UNIFICATION *) 
      let prj = bexpr_aprj f ta t in
      bexpr_apply t (prj,a)
    | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  (* ---------------------------------------------------------- *)
  (* special case, array pointer projection  *) 
  (* ---------------------------------------------------------- *)
  try
    let (bf,tf) as f = 
      try be f' 
      with
      | Flx_exceptions.SimpleNameNotFound _ as x -> raise x
      | exn -> raise Flx_dot.OverloadResolutionError 
    in
    match tf, ta with
    (* Check for array projection *)
    | ixt1, BTYP_pointer (BTYP_array (base,ixt2) as vt) when ixt1 = ixt2 -> (* SHOULD USE UNIFICATION *) 
(*
print_endline ("array pointer projection, base type " ^ Flx_print.sbt bsym_table base);
*)
      let pt = if iscompact_linear_product vt then btyp_cltpointer vt base else btyp_pointer base in
(*
print_endline ("   array pointer projection, index type " ^ Flx_print.sbt bsym_table ixt1);
print_endline ("   array pointer projection, codomain type " ^ Flx_print.sbt bsym_table pt);
*)
      let prj = bexpr_aprj f ta pt in
      bexpr_apply pt (prj,a)
    | ixt1, BTYP_rref(BTYP_array (base,ixt2) as vt) when ixt1 = ixt2 -> (* SHOULD USE UNIFICATION *) 
      let pt = if iscompact_linear_product vt then btyp_cltrref vt base else btyp_rref base in
      let prj = bexpr_aprj f ta pt in
      bexpr_apply pt (prj,a)
    | ixt1, BTYP_wref (BTYP_array (base,ixt2) as vt) when ixt1 = ixt2 -> (* SHOULD USE UNIFICATION *) 
      let pt = if iscompact_linear_product vt then btyp_cltwref vt base else btyp_wref base in
      let prj = bexpr_aprj f ta pt in
      bexpr_apply pt (prj,a)
    | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  raise Flx_exceptions.TryNext


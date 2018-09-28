open Flx_btype
open Flx_bexpr
(* NOTE: this is crud. It mixes record projection application with
  struct field name projections. Split up!
*)

let try_bind_record_proj 
  bsym_table state build_env koenig_lookup be bt env rs cal_apply bind_type' mkenv
  f' a' (ea,ta as a) sr name ts 
=
match unfold "flx_lookup" ta with 
(* record and polyrecord value *)
| BTYP_polyrecord (es,_)
| BTYP_record (es) ->
  if (ts != []) then raise Flx_dot.OverloadResolutionError; 
  let k = List.length es in
  let field_name = name in
  begin match Flx_list.list_index (List.map fst es) field_name with
  | Some n -> 
    let t = List.assoc field_name es in
    let t = bexpr_get_named t name a in
    t
  | None -> 
    raise Flx_dot.OverloadResolutionError
  end

(* pointer to record *)
| BTYP_pointer (BTYP_record _ as r) 
| BTYP_pointer (BTYP_polyrecord _ as r) ->
  begin match unfold "flx_lookup" r with
  | BTYP_polyrecord (es,_) 
  | BTYP_record (es) ->
    if (ts != []) then raise Flx_dot.OverloadResolutionError; 
    let k = List.length es in
    let field_name = name in
    begin match Flx_list.list_index (List.map fst es) field_name with
    | Some n -> 
      let t = List.assoc field_name es in
      let t = bexpr_get_named (btyp_pointer t) name a in
      t
    | None -> 
      raise Flx_dot.OverloadResolutionError
    end
  | _ -> assert false
  end

(* Instance *)
| BTYP_inst (i,ts',_) ->
  begin try
  Flx_dot.handle_field_name state bsym_table build_env env rs 
    be bt koenig_lookup cal_apply bind_type' mkenv 
    sr a' f' name ts i ts' false
  with Flx_dot.Not_field -> raise Flx_dot.OverloadResolutionError
  end

(* pointer to instance *)
| BTYP_pointer (BTYP_inst (i,ts',_) as r) ->
(* NOTE: This may not work, unfold doesn't penetrate into a struct!
However, if the struct is complete but polymorphic, it should work
by unfolding the ts values ..
*)
  begin match unfold "flx_lookup:bind_expression:btyp_inst" r with | BTYP_inst (i,ts',_) ->
    begin try
    Flx_dot.handle_field_name state bsym_table build_env env rs 
      be bt koenig_lookup cal_apply bind_type' mkenv 
      sr a' f' name ts i ts' true 
    with Flx_dot.Not_field -> raise Flx_dot.OverloadResolutionError
    end
  | _ -> assert false
  end
| _ -> raise Flx_dot.OverloadResolutionError 



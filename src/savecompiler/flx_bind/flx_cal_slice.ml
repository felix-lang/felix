open Flx_ast

let cal_slice tmin tmax slice =
  let smin, smax = 
  match slice with
    | EXPR_name (_,"Slice_all",[]) -> tmin,tmax
    | EXPR_name (_,"Slice_none",[]) -> tmax,tmin
    | EXPR_apply(_,(EXPR_name (_,"Slice_from",[]), EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
      int_of_string v1,tmax
    | EXPR_apply(_,(EXPR_name (_,"Slice_from_counted",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})]))) -> (* second arg is count *)
      int_of_string v1,int_of_string v1+int_of_string v2-1
    | EXPR_apply(_,(EXPR_name (_,"Slice_to_incl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
      0,int_of_string v1
    | EXPR_apply(_,(EXPR_name (_,"Slice_to_excl",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) ->
      0,int_of_string v1 - 1
    | EXPR_apply(_,(EXPR_name (_,"Slice_range_incl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
      int_of_string v1,int_of_string v2
    | EXPR_apply(_,(EXPR_name (_,"Slice_range_excl",[]),EXPR_tuple(_,[EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1});EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v2})])))->
      int_of_string v1,int_of_string v2 - 1
    | EXPR_apply(_,(EXPR_name (_,"Slice_one",[]),EXPR_literal (_,{Flx_literal.felix_type="int"; internal_value=v1}))) -> 
      int_of_string v1,int_of_string v1
    | _ -> raise Flx_dot.OverloadResolutionError
  in
  let smin = max tmin smin and smax = min tmax smax  in
  smin,smax


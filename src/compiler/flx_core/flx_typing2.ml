open Flx_ast
open Flx_types
open Flx_print
open Flx_exceptions
open List

let qualified_name_of_expr e =
  match e with
  | `EXPR_name (sr,name,ts) -> Some (`AST_name (sr,name,ts))
  | `EXPR_case_tag (sr,v) -> Some (`AST_case_tag (sr,v))
  | `EXPR_typed_case (sr,v,t) -> Some (`AST_typed_case (sr,v,t))
  | `EXPR_lookup (sr,(e,name,ts)) -> Some (`AST_lookup (sr,(e,name,ts)))
  | `EXPR_index (sr,name,index) -> Some (`AST_index (sr,name,index))
  | `EXPR_callback (sr,name) -> Some (`AST_callback (sr,name))
  | _ -> None

let suffixed_name_of_expr e =
  match e with
  | `EXPR_suffix (sr,(name,ts)) -> Some (`AST_suffix (sr,(name,ts)))
  | _ -> qualified_name_of_expr e

let expr_of_qualified_name e =
  match e with
  | `AST_name (sr,name,ts) -> `EXPR_name (sr,name,ts)
  | `AST_case_tag (sr,v) -> `EXPR_case_tag (sr,v)
  | `AST_typed_case (sr,v,t) -> `EXPR_typed_case (sr,v,t)
  | `AST_lookup (sr,(e,name,ts)) -> `EXPR_lookup (sr,(e,name,ts))
  | `AST_index (sr,name,index) -> `EXPR_index (sr,name,index)
  | `AST_callback (sr,name) -> `EXPR_callback (sr,name)

let expr_of_suffixed_name e =
  match e with
  (* | `AST_void sr -> `EXPR_void sr *)
  | `AST_name (sr,name,ts) -> `EXPR_name (sr,name,ts)
  | `AST_case_tag (sr,v) -> `EXPR_case_tag (sr,v)
  | `AST_typed_case (sr,v,t) -> `EXPR_typed_case (sr,v,t)
  | `AST_lookup (sr,(e,name,ts)) -> `EXPR_lookup (sr,(e,name,ts))
  | `AST_index (sr,name,index) -> `EXPR_index (sr,name,index)
  | `AST_callback (sr,name) -> `EXPR_callback (sr,name)
  | `AST_suffix (sr,(name,ts)) -> `EXPR_suffix (sr,(name,ts))

let type_of_list = function
  | [x] -> x
  | x -> `TYP_tuple x

let kind_of_list = function
  | [x] -> x
  | x -> KND_tuple x

let all_tunits ts =
  try
    iter
    (fun t ->
      if t <> `TYP_tuple []
      then raise Not_found
    )
    ts;
    true
  with Not_found -> false


let string_of_type_name (t:typecode_t) = match t with
  | `TYP_bool _ -> "`TYP_bool"
  | `TYP_rptsum _ -> "`TYP_rptsum"
  | `TYP_pclt _ -> "`TYP_pclt"
  | `TYP_rpclt _ -> "`TYP_rpclt"
  | `TYP_wpclt _ -> "`TYP_wpclt"
  | `TYP_label -> "`TYP_label"
  | `TYP_none -> " `TYP_none"
  | `TYP_ellipsis -> "`TYP_ellipsis"
  | `TYP_void _ -> "`TYP_void"
  | `TYP_name _ -> " `TYP_name"
  | `TYP_case_tag _ -> " `TYP_case_tag"
  | `TYP_lookup _ -> " `TYP_lookup"
  | `TYP_index _ -> " `TYP_index"
  | `TYP_callback _ -> " `TYP_callback"
  | `TYP_suffix _ -> " `TYP_suffix"
  | `TYP_patvar _ -> " `TYP_patvar"
  | `TYP_patany _ -> " `TYP_patany"
  | `TYP_tuple _ -> "`TYP_tuple"
  | `TYP_unitsum _ -> "`TYP_unitsum"
  | `TYP_sum _ -> "`TYP_sum"
  | `TYP_intersect _ -> "`TYP_intersect"
  | `TYP_union _ -> "`TYP_union"
  | `TYP_record _ -> "`TYP_record"
  | `TYP_polyrecord _ -> "`TYP_polyrecord"
  | `TYP_variant _ -> "`TYP_variant"
  | `TYP_function _ -> "`TYP_function"
  | `TYP_effector _ -> "`TYP_effector"
  | `TYP_cfunction _ -> "`TYP_cfunction"
  | `TYP_pointer _ -> "`TYP_pointer"
  | `TYP_rref _ -> "`TYP_rref"
  | `TYP_wref _ -> "`TYP_wref"
  | `TYP_uniq _-> "`TYP_uniq"
  | `TYP_array _ -> "`TYP_array"
  | `TYP_as _ -> "`TYP_as"
  | `TYP_var _ -> "`TYP_var"
  | `TYP_isin _ -> "`TYP_isin"
  | `TYP_defer _ -> "`TYP_defer"
  | `TYP_typeset _ -> "`TYP_typeset"
  | `TYP_setunion _ -> "`TYP_setunion"
  | `TYP_setintersection _ -> "`TYP_setintersection"
  | `TYP_dual _ -> "`TYP_dual"
  | `TYP_apply _ -> "`TYP_apply"
  | `TYP_typefun _ -> "`TYP_typefun"
  | `TYP_type_tuple _ -> "`TYP_type_tuple"
  | `TYP_type_match _ -> "`TYP_type_match"
  | `TYP_subtype_match _ -> "`TYP_subtype_match"
  | `TYP_type_extension _ -> "`TYP_type_extension"
  | `TYP_tuple_cons _ -> "`TYP_tuple_cons"
  | `TYP_tuple_snoc _ -> "`TYP_tuple_snoc"
  | `TYP_typeof _ -> "`TYP_typeof"
  | `TYP_typeop _ -> "`TYP_typeop"

open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_typing

let ident x = x

let map_type f (t:typecode_t):typecode_t = match t with
  | TYP_defer (sr,dt) -> 
    begin match !dt with
    | None -> t (* unmodified *)
    | Some xt -> f xt  (* replace defered type completely *)
    end
  | TYP_name (sr,name,ts) -> TYP_name (sr, name, List.map f ts)
  | TYP_lookup (sr,(e,name,ts)) -> TYP_lookup (sr, (e, name, List.map f ts))
  | TYP_suffix (sr,(qn,t)) -> TYP_suffix (sr, (qn, f t))
  | TYP_typed_case (sr,i,t) -> TYP_typed_case (sr,i, f t)
  | TYP_tuple ts -> TYP_tuple (List.map f ts)
  | TYP_record ts -> TYP_record (List.map (fun (s,t) -> s,f t) ts)
  | TYP_polyrecord (ts,v) -> TYP_polyrecord (List.map (fun (s,t) -> s,f t) ts, f v)
  | TYP_variant ts -> TYP_variant (List.map (fun (s,t) -> s,f t) ts)
  | TYP_isin (a,b) -> TYP_isin (f a, f b)

  (* we have to do this, so that a large unitsum
     can be specified without overflowing the compiler
     storage
  *)
  | TYP_unitsum k ->
    if k>0 then
      let mapped_unit = f (TYP_tuple []) in
      match mapped_unit with
      | TYP_tuple [] ->
        TYP_unitsum k
      | _ -> TYP_tuple (Flx_list.repeat mapped_unit k)
    else TYP_unitsum k

  (* here we don't need to go to a unitsum, since
     we have already used up storage
  *)
  | TYP_sum ts -> TYP_sum (List.map f ts)
  | TYP_intersect ts -> TYP_intersect (List.map f ts)
  | TYP_function (a,b) -> TYP_function (f a, f b)
  | TYP_cfunction (a,b) -> TYP_cfunction (f a, f b)
  | TYP_pointer t -> TYP_pointer (f t)
  | TYP_array (t1, t2) -> TYP_array (f t1, f t2)
  | TYP_as (t,s) -> TYP_as (f t,s)

  (* type sets *)
  | TYP_typeset ts -> TYP_typeset (List.map f ts)
  | TYP_setintersection ts -> TYP_setintersection (List.map f ts)
  | TYP_setunion ts -> TYP_setunion (List.map f ts)

  (* dualizer *)
  | TYP_dual t -> TYP_dual (f t)

  (*
  | TYP_type_match (t,ps) ->
    let ps = List.map (fun (p,t) -> p, f t) ps in
    TYP_type_match (f t, ps)
  *)
  | TYP_type_match (t,ps) ->
    let ps = List.map (fun (p,t) -> f p, f t) ps in
    TYP_type_match (f t, ps)

  | TYP_tuple_cons (sr,t1,t2) -> TYP_tuple_cons (sr, f t1, f t2)
  (* meta constructors *)
  | TYP_apply (a,b) -> TYP_apply (f a, f b)
  | TYP_typefun (ps, a, b) -> TYP_typefun (ps, f a, f b)
  | TYP_type_tuple ts -> TYP_type_tuple (List.map f ts)
  | TYP_type_extension (sr,ts,t) -> TYP_type_extension (sr,List.map f ts, f t)


  (* invariant ..?? *)
  | TYP_typeof _
  | TYP_callback _
  | TYP_case_tag _
  | TYP_index _
  | TYP_var _
  | TYP_patvar _
  | TYP_patany _

  (* absolute constants *)
  | TYP_label
  | TYP_void _
  | TYP_ellipsis
  | TYP_type
  | TYP_none
  -> t


let map_expr f (e:expr_t):expr_t = match e with
  | EXPR_label _
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_interpolate _ 
  | EXPR_vsprintf _ -> e
  | EXPR_map (sr,a,b) -> EXPR_map (sr,f a, f b)
  | EXPR_noexpand (sr,x) -> e (* DO NOT EXPAND .. HMM .. *)
  | EXPR_name _ -> e
  | EXPR_callback _ -> e
  | EXPR_index _ -> e
  | EXPR_case_tag _ -> e
  | EXPR_typed_case _ -> e
  | EXPR_projection _ -> e
  | EXPR_rnprj (sr,name,seq,a) -> EXPR_rnprj (sr,name,seq, f a)
  | EXPR_lookup (sr,(x,s,ts)) -> EXPR_lookup (sr,(f x, s, ts))
  | EXPR_apply (sr,(a,b)) -> EXPR_apply (sr,(f a, f b))
  | EXPR_tuple (sr,es) -> EXPR_tuple (sr, List.map f es)
  | EXPR_tuple_cons (sr,eh, et) -> EXPR_tuple_cons (sr, f eh, f et)
  | EXPR_record (sr,es) -> EXPR_record (sr, List.map (fun (s,e) -> s,f e) es)
  | EXPR_polyrecord (sr,es,e) -> EXPR_polyrecord (sr, List.map (fun (s,e) -> s,f e) es, f e)
  | EXPR_remove_fields (sr,e,ss) -> EXPR_remove_fields (sr, f e, ss)
  | EXPR_variant (sr,(s,e)) -> EXPR_variant (sr, (s,f e))
  | EXPR_arrayof (sr, es) -> EXPR_arrayof (sr, List.map f es)
  | EXPR_coercion (sr, (x,t)) -> EXPR_coercion (sr,(f x, t))
  | EXPR_suffix _ -> e

  | EXPR_record_type (sr,ts) -> e
  | EXPR_polyrecord_type (sr,ts,v) -> e
  | EXPR_variant_type (sr,ts) -> e
  | EXPR_void sr -> e
  | EXPR_ellipsis sr -> e
  | EXPR_product (sr,es) -> EXPR_product (sr, List.map f es)
  | EXPR_sum (sr,es) -> EXPR_sum (sr, List.map f es)
  | EXPR_intersect (sr,es) -> EXPR_intersect (sr, List.map f es)
  | EXPR_isin (sr,(a,b)) -> EXPR_isin (sr, (f a, f b))
  | EXPR_orlist (sr,es) -> EXPR_orlist (sr, List.map f es)
  | EXPR_andlist (sr,es) -> EXPR_andlist (sr, List.map f es)
  | EXPR_arrow (sr,(a,b)) -> EXPR_arrow (sr,(f a, f b))
  | EXPR_longarrow (sr,(a,b)) -> EXPR_longarrow (sr,(f a, f b))
  | EXPR_superscript (sr,(a,b)) -> EXPR_superscript (sr,(f a, f b))

  | EXPR_literal _ -> e
  | EXPR_deref (sr,x) -> EXPR_deref (sr,f x)
  | EXPR_ref (sr,x) -> EXPR_ref (sr, f x)
  | EXPR_likely (sr,x) -> EXPR_likely (sr, f x)
  | EXPR_unlikely (sr,x) -> EXPR_unlikely (sr, f x)
  | EXPR_new (sr,x) -> EXPR_new (sr, f x)

  (* GIVE UP ON LAMBDAS FOR THE MOMENT .. NEEDS STATEMENT MAPPING TOO *)
  (* | EXPR_lambda of Flx_srcref.t * (vs_list_t * params_t list * typecode_t * statement_t list) *)
  | EXPR_lambda _ -> e

  | EXPR_match_ctor (sr,(qn,x)) -> EXPR_match_ctor (sr,(qn,f x))
  | EXPR_match_variant (sr,(s,x)) -> EXPR_match_variant (sr,(s,f x))
  | EXPR_match_case (sr,(j,x)) -> EXPR_match_case (sr,(j, f x))

  | EXPR_ctor_arg (sr,(qn,x)) -> EXPR_ctor_arg (sr,(qn,f x))
  | EXPR_variant_arg (sr,(s,x)) -> EXPR_variant_arg (sr,(s, f x))
  | EXPR_case_arg (sr,(j,x)) -> EXPR_case_arg (sr,(j, f x))
  | EXPR_case_index (sr,x) -> EXPR_case_index (sr,f x)

  | EXPR_letin (sr,(pat,a,b)) -> EXPR_letin (sr,(pat,f a, f b))

  | EXPR_get_n (sr,(j,x)) -> EXPR_get_n (sr,(j,f x))
  | EXPR_get_named_variable (sr,(j,x)) -> EXPR_get_named_variable (sr,(j,f x))
  | EXPR_as (sr,(x,s)) -> EXPR_as (sr,(f x, s))
  | EXPR_as_var (sr,(x,s)) -> EXPR_as_var (sr,(f x, s))
  | EXPR_match (sr,(a,pes)) ->
    EXPR_match (sr, (f a, List.map (fun (pat,x) -> pat, f x) pes))

  | EXPR_typeof (sr,x) -> EXPR_typeof (sr,f x)
  | EXPR_cond (sr,(a,b,c)) -> EXPR_cond (sr, (f a, f b, f c))

  | EXPR_expr (sr,s,t,e) -> EXPR_expr (sr,s,t, f e)
  | EXPR_type_match _ -> e
  | EXPR_typecase_match (sr,(e,ps)) ->
    let ps = List.map (fun (t,e) -> t, f e) ps in
    EXPR_typecase_match (sr, (f e, ps))

  | EXPR_range_check (sr,mi,v,mx) -> EXPR_range_check (sr, f mi, f v, f mx)
  | EXPR_not (sr,e) -> EXPR_not (sr, f e)
  | EXPR_extension (sr,es,e) -> EXPR_extension (sr, List.map f es, f e)
  | EXPR_get_tuple_tail (sr,e) -> EXPR_get_tuple_tail (sr, f e)
  | EXPR_get_tuple_head (sr,e) -> EXPR_get_tuple_head (sr, f e)
 
 
let iter_expr f (e:expr_t) =
  f e;
  match e with
  | EXPR_label _
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_interpolate _
  | EXPR_vsprintf _
  | EXPR_name _
  | EXPR_callback _
  | EXPR_index _
  | EXPR_case_tag _
  | EXPR_typed_case _
  | EXPR_projection _
  | EXPR_record_type _
  | EXPR_polyrecord_type _
  | EXPR_variant_type _
  | EXPR_void _
  | EXPR_ellipsis _
  | EXPR_noexpand _
  | EXPR_suffix _
  | EXPR_literal _
  | EXPR_lambda _
  | EXPR_type_match _
    -> ()

  | EXPR_expr (_,_,_,x)
  | EXPR_rnprj (_,_,_,x)
  | EXPR_variant (_,(_,x))
  | EXPR_typeof (_,x)
  | EXPR_as (_,(x,_))
  | EXPR_as_var (_,(x,_))
  | EXPR_get_n (_,(_,x))
  | EXPR_get_named_variable (_,(_,x))
  | EXPR_ctor_arg (_,(_,x))
  | EXPR_variant_arg (_,(_,x))
  | EXPR_case_arg (_,(_,x))
  | EXPR_case_index (_,x)
  | EXPR_match_ctor (_,(_,x))
  | EXPR_match_variant (_,(_,x))
  | EXPR_match_case (_,(_,x))
  | EXPR_deref (_,x)
  | EXPR_ref (_,x)
  | EXPR_likely (_,x)
  | EXPR_unlikely (_,x)
  | EXPR_new (_,x)
  | EXPR_lookup (_,(x,_,_))
  | EXPR_coercion (_, (x,_))
  | EXPR_not (_,x) 
  | EXPR_get_tuple_tail (_,x)
  | EXPR_get_tuple_head (_,x)
  | EXPR_remove_fields (_,x,_)
    -> f x

  | EXPR_letin (_,(_,a,b))
  | EXPR_longarrow (_,(a,b))
  | EXPR_superscript (_,(a,b))
  | EXPR_arrow (_,(a,b))
  | EXPR_map (_,a,b)
  | EXPR_apply (_,(a,b))
  | EXPR_isin (_,(a,b))
  | EXPR_tuple_cons (_, a, b) 
    -> f a; f b

  | EXPR_tuple (_,es)
  | EXPR_product (_,es)
  | EXPR_sum (_,es)
  | EXPR_intersect (_,es)
  | EXPR_orlist (_,es)
  | EXPR_andlist (_,es)
  | EXPR_arrayof (_, es) ->
    List.iter f es

  | EXPR_record (sr,es) -> List.iter (fun (s,e) -> f e) es
  | EXPR_polyrecord (sr,es,e) -> List.iter (fun (s,e) -> f e) es; f e

  | EXPR_match (sr,(a,pes)) ->
    f a; List.iter (fun (pat,x) -> f x) pes

  | EXPR_typecase_match (sr,(e,es)) ->
    f e; List.iter (fun (t,e) -> f e) es

  | EXPR_cond (sr,(a,b,c)) -> f a; f b; f c
  | EXPR_range_check (sr,a,b,c) -> f a; f b; f c
  | EXPR_extension (sr,es,e) -> List.iter f es; f e

let scan_expr e =
  let ls = ref [] in
  let add x = ls := Flx_ast.src_of_expr x :: !ls in
  iter_expr add e;
  Flx_list.uniq_list !ls

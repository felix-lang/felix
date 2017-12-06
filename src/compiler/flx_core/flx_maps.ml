open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_typing

let ident x = x

let map_type f (t:typecode_t):typecode_t = match t with
  | TYP_rptsum (i,t) -> TYP_rptsum (f i, f t)
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
  | TYP_variant ts -> TYP_variant (List.map (
      fun x -> match x with `Ctor (s,t) -> `Ctor (s, f t) | `Base t -> `Base (f t)
      ) ts)
  | TYP_isin (a,b) -> TYP_isin (f a, f b)
  | TYP_pclt (a,b) -> TYP_pclt (f a, f b)

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
  | TYP_union ts -> TYP_union (List.map f ts)
  | TYP_function (a,b) -> TYP_function (f a, f b)
  | TYP_effector (a,e,b) -> TYP_effector (f a, f e, f b)
  | TYP_cfunction (a,b) -> TYP_cfunction (f a, f b)
  | TYP_pointer t -> TYP_pointer (f t)
  | TYP_rref t -> TYP_rref (f t)
  | TYP_wref t -> TYP_wref (f t)
  | TYP_uniq t -> TYP_uniq (f t)
  | TYP_array (t1, t2) -> TYP_array (f t1, f t2)
  | TYP_as (t,s) -> TYP_as (f t,s)

  (* type sets *)
  | TYP_typeset ts -> TYP_typeset (List.map f ts)
  | TYP_setintersection ts -> TYP_setintersection (List.map f ts)
  | TYP_setunion ts -> TYP_setunion (List.map f ts)

  (* dualizer *)
  | TYP_dual t -> TYP_dual (f t)

  | TYP_type_match (t,ps) ->
    let ps = List.map (fun (p,t) -> f p, f t) ps in
    TYP_type_match (f t, ps)

  | TYP_subtype_match (t,ps) ->
    let ps = List.map (fun (p,t) -> f p, f t) ps in
    TYP_subtype_match (f t, ps)


  | TYP_tuple_cons (sr,t1,t2) -> TYP_tuple_cons (sr, f t1, f t2)
  | TYP_tuple_snoc (sr,t1,t2) -> TYP_tuple_snoc (sr, f t1, f t2)

  (* meta constructors *)
  | TYP_apply (a,b) -> TYP_apply (f a, f b)
  | TYP_typefun (ps, a, b) -> TYP_typefun (ps, a, f b)
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
  | TYP_none
  -> t


let full_map_expr fi ft fe (e:expr_t):expr_t = match e with
  | EXPR_rptsum_arg (sr, e) -> EXPR_rptsum_arg (sr, fe e)
  | EXPR_label _
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_interpolate _ 
  | EXPR_vsprintf _ -> e
  | EXPR_map (sr,a,b) -> EXPR_map (sr,fe a, fe b)
  | EXPR_noexpand (sr,x) -> e (* DO NOT EXPAND .. HMM .. *)
  | EXPR_name (sr,name,ts) -> EXPR_name (sr, name, List.map ft ts) 
  | EXPR_callback _ -> e
  | EXPR_index (sr,s,ix) -> 
(*
    print_endline ("full_map_expr EXPR_index " ^ string_of_int ix ^ " -> " ^ string_of_int (fi ix)); 
*)
    EXPR_index (sr,s,fi ix)
  | EXPR_case_tag _ -> e
  | EXPR_typed_case (sr,n,t) -> EXPR_typed_case (sr,n, ft t)
  | EXPR_rptsum_type (sr,n,t) -> EXPR_rptsum_type (sr, ft n, ft t)
  | EXPR_projection (sr,n,t) -> EXPR_projection (sr,n,ft t)
  | EXPR_ainj (sr,n,t) -> EXPR_ainj (sr,fe n,ft t)
  | EXPR_rnprj (sr,name,seq,a) -> EXPR_rnprj (sr,name,seq, fe a)
  | EXPR_lookup (sr,(x,s,ts)) -> EXPR_lookup (sr,(fe x, s, List.map ft ts))
  | EXPR_apply (sr,(a,b)) -> EXPR_apply (sr,(fe a, fe b))
  | EXPR_tuple (sr,es) -> EXPR_tuple (sr, List.map fe es)
  | EXPR_record (sr,es) -> EXPR_record (sr, List.map (fun (s,e) -> s,fe e) es)
  | EXPR_polyrecord (sr,es,e) -> EXPR_polyrecord (sr, List.map (fun (s,e) -> s,fe e) es, fe e)
  | EXPR_replace_fields (sr,e,ss) -> 
    EXPR_replace_fields (sr, fe e, List.map (fun (s,e) -> s,fe e) ss)
  | EXPR_remove_fields (sr,e,ss) -> EXPR_remove_fields (sr, fe e, ss)
  | EXPR_variant (sr,(s,e)) -> EXPR_variant (sr, (s,fe e))
  | EXPR_arrayof (sr, es) -> EXPR_arrayof (sr, List.map fe es)
  | EXPR_coercion (sr, (x,t)) -> EXPR_coercion (sr,(fe x, ft t))
  | EXPR_variant_subtype_match_coercion (sr, (x,t)) -> EXPR_variant_subtype_match_coercion (sr,(fe x, ft t))
  | EXPR_suffix (sr,(qn,t)) -> EXPR_suffix (sr,(qn, ft t))
  | EXPR_uniq (sr,e) -> EXPR_uniq (sr, fe e)


  (* these ones are types and should have been desugared out a long way back *)
  (* BUT Garrets code reintroduces them .. *)
(*
  | EXPR_record_type (sr,ts) -> assert false 
  | EXPR_polyrecord_type (sr,ts,v) -> assert false
  | EXPR_variant_type (sr,ts) -> assert false
  | EXPR_void sr -> assert false 
*)

  | EXPR_pclt_type (sr,a,b) -> EXPR_pclt_type (sr, ft a, ft b)

  | EXPR_record_type (sr,ts) -> EXPR_record_type (sr, List.map (fun (s,t) -> s, ft t) ts) 
  | EXPR_polyrecord_type (sr,ts,v) -> EXPR_polyrecord_type (sr, List.map (fun (s,t)-> s,ft t) ts, ft v)
  | EXPR_variant_type (sr,ts) -> EXPR_variant_type (sr, List.map (
      fun x -> match x with | `Ctor (s,t) -> `Ctor (s, ft t) | `Base t -> `Base (ft t)
    ) ts)
  | EXPR_void sr -> EXPR_void sr

  | EXPR_ellipsis sr -> e
  | EXPR_product (sr,es) -> EXPR_product (sr, List.map fe es)
  | EXPR_sum (sr,es) -> EXPR_sum (sr, List.map fe es)
  | EXPR_intersect (sr,es) -> EXPR_intersect (sr, List.map fe es)
  | EXPR_union (sr,es) -> EXPR_union(sr, List.map fe es)
  | EXPR_isin (sr,(a,b)) -> EXPR_isin (sr, (fe a, fe b))
  | EXPR_orlist (sr,es) -> EXPR_orlist (sr, List.map fe es)
  | EXPR_andlist (sr,es) -> EXPR_andlist (sr, List.map fe es)
  | EXPR_arrow (sr,(a,b)) -> EXPR_arrow (sr,(fe a, fe b))
  | EXPR_effector (sr,(a,e,b)) -> EXPR_effector (sr,(fe a, fe e, fe b))
  | EXPR_longarrow (sr,(a,b)) -> EXPR_longarrow (sr,(fe a, fe b))
  | EXPR_superscript (sr,(a,b)) -> EXPR_superscript (sr,(fe a, fe b))

  | EXPR_literal _ -> e
  | EXPR_deref (sr,x) -> EXPR_deref (sr,fe x)
  | EXPR_ref (sr,x) -> EXPR_ref (sr, fe x)
  | EXPR_rref (sr,x) -> EXPR_rref (sr, fe x)
  | EXPR_wref (sr,x) -> EXPR_wref (sr, fe x)
  | EXPR_likely (sr,x) -> EXPR_likely (sr, fe x)
  | EXPR_unlikely (sr,x) -> EXPR_unlikely (sr, fe x)
  | EXPR_new (sr,x) -> EXPR_new (sr, fe x)

  (* GIVE UP ON LAMBDAS FOR THE MOMENT .. NEEDS STATEMENT MAPPING TOO *)
  (* | EXPR_lambda of Flx_srcref.t * (vs_list_t * params_t list * typecode_t * statement_t list) *)
  | EXPR_lambda _ -> e

  | EXPR_match_variant_subtype (sr,(e,t)) -> EXPR_match_variant_subtype (sr,(fe e, ft t))
  | EXPR_match_ctor (sr,(qn,x)) -> EXPR_match_ctor (sr,(qn,fe x))
  | EXPR_match_ho_ctor (sr,(qn,es)) -> EXPR_match_ho_ctor (sr,(qn,List.map fe es))
  | EXPR_match_variant (sr,(s,x)) -> EXPR_match_variant (sr,(s,fe x))
  | EXPR_match_case (sr,(j,x)) -> EXPR_match_case (sr,(j, fe x))

  | EXPR_ctor_arg (sr,(qn,x)) -> EXPR_ctor_arg (sr,(qn,fe x))
  | EXPR_ho_ctor_arg (sr,(qn,es)) -> EXPR_ho_ctor_arg (sr,(qn,List.map fe es))
  | EXPR_variant_arg (sr,(s,x)) -> EXPR_variant_arg (sr,(s, fe x))
  | EXPR_case_arg (sr,(j,x)) -> EXPR_case_arg (sr,(j, fe x))
  | EXPR_case_index (sr,x) -> EXPR_case_index (sr,fe x)

  | EXPR_letin (sr,(pat,a,b)) -> EXPR_letin (sr,(pat,fe a, fe b))

  | EXPR_get_n (sr,(j,x)) -> EXPR_get_n (sr,(j,fe x))
  | EXPR_get_named_variable (sr,(j,x)) -> EXPR_get_named_variable (sr,(j,fe x))
  | EXPR_as (sr,(x,s)) -> EXPR_as (sr,(fe x, s))
  | EXPR_as_var (sr,(x,s)) -> EXPR_as_var (sr,(fe x, s))
  | EXPR_match (sr,(a,pes)) ->
    EXPR_match (sr, (fe a, List.map (fun (pat,x) -> pat, fe x) pes))

  | EXPR_typeof (sr,x) -> EXPR_typeof (sr,fe x)
  | EXPR_cond (sr,(a,b,c)) -> EXPR_cond (sr, (fe a, fe b, fe c))

  | EXPR_expr (sr,s,t,e) -> EXPR_expr (sr,s,ft t, fe e)
  | EXPR_type_match _ -> e
  | EXPR_subtype_match _ -> e
  | EXPR_typecase_match (sr,(t,ps)) ->
    let ps = List.map (fun (t,e) -> ft t, fe e) ps in
    EXPR_typecase_match (sr, (ft t, ps))

  | EXPR_range_check (sr,mi,v,mx) -> EXPR_range_check (sr, fe mi, fe v, fe mx)
  | EXPR_not (sr,e) -> EXPR_not (sr, fe e)
  | EXPR_extension (sr,es,e) -> EXPR_extension (sr, List.map fe es, fe e)

  | EXPR_tuple_cons (sr,eh, et) -> EXPR_tuple_cons (sr, fe eh, fe et)
  | EXPR_get_tuple_tail (sr,e) -> EXPR_get_tuple_tail (sr, fe e)
  | EXPR_get_tuple_head (sr,e) -> EXPR_get_tuple_head (sr, fe e)

  | EXPR_tuple_snoc (sr,eh, et) -> EXPR_tuple_snoc (sr, fe eh, fe et)
  | EXPR_get_tuple_body (sr,e) -> EXPR_get_tuple_body (sr, fe e)
  | EXPR_get_tuple_last (sr,e) -> EXPR_get_tuple_last (sr, fe e)


let idf x = x 
let map_expr fe (e:expr_t):expr_t = full_map_expr idf idf fe e
 
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
  | EXPR_subtype_match _
  | EXPR_pclt_type _
  | EXPR_rptsum_type _
    -> ()

  | EXPR_ainj (_,x,_)
  | EXPR_rptsum_arg (_,x)
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
  | EXPR_match_variant_subtype (_,(x,_))
  | EXPR_match_ctor (_,(_,x))
  | EXPR_match_variant (_,(_,x))
  | EXPR_match_case (_,(_,x))
  | EXPR_deref (_,x)
  | EXPR_ref (_,x)
  | EXPR_rref (_,x)
  | EXPR_wref (_,x)
  | EXPR_likely (_,x)
  | EXPR_unlikely (_,x)
  | EXPR_new (_,x)
  | EXPR_lookup (_,(x,_,_))
  | EXPR_coercion (_, (x,_))
  | EXPR_variant_subtype_match_coercion (_, (x,_))
  | EXPR_not (_,x) 
  | EXPR_get_tuple_tail (_,x)
  | EXPR_get_tuple_head (_,x)
  | EXPR_get_tuple_body (_,x)
  | EXPR_get_tuple_last (_,x)
  | EXPR_remove_fields (_,x,_)
  | EXPR_uniq (_,x)
    -> f x

  | EXPR_letin (_,(_,a,b))
  | EXPR_longarrow (_,(a,b))
  | EXPR_superscript (_,(a,b))
  | EXPR_arrow (_,(a,b))
  | EXPR_map (_,a,b)
  | EXPR_apply (_,(a,b))
  | EXPR_isin (_,(a,b))
  | EXPR_tuple_cons (_, a, b) 
  | EXPR_tuple_snoc (_, a, b) 
    -> f a; f b

  | EXPR_effector (_,(a,e,b))
    -> f a; f e; f b

  | EXPR_tuple (_,es)
  | EXPR_product (_,es)
  | EXPR_sum (_,es)
  | EXPR_intersect (_,es)
  | EXPR_union (_,es)
  | EXPR_orlist (_,es)
  | EXPR_andlist (_,es)
  | EXPR_arrayof (_, es)
  | EXPR_match_ho_ctor (_,(_,es))
  | EXPR_ho_ctor_arg (_,(_,es)) ->
    List.iter f es

  | EXPR_record (sr,es) -> List.iter (fun (s,e) -> f e) es
  | EXPR_polyrecord (sr,es,e) -> List.iter (fun (s,e) -> f e) es; f e
  | EXPR_replace_fields (sr,e,es) -> List.iter (fun (s,e) -> f e) es; f e

  | EXPR_match (sr,(a,pes)) ->
    f a; List.iter (fun (pat,x) -> f x) pes

  | EXPR_typecase_match (sr,(t,es)) ->
    List.iter (fun (t,e) -> f e) es

  | EXPR_cond (sr,(a,b,c)) -> f a; f b; f c
  | EXPR_range_check (sr,a,b,c) -> f a; f b; f c
  | EXPR_extension (sr,es,e) -> List.iter f es; f e

let scan_expr e =
  let ls = ref [] in
  let add x = ls := Flx_ast.src_of_expr x :: !ls in
  iter_expr add e;
  Flx_list.uniq_list !ls

let rec map_exe fi ft fe (x:exe_t):exe_t = match x with
  | EXE_begin_match_case
  | EXE_end_match_case -> x

  | EXE_circuit cs -> x 
  | EXE_type_error (x) -> EXE_type_error (map_exe fi ft fe x)
  | EXE_type_assert (x) -> EXE_type_assert (map_exe fi ft fe x)
  | EXE_code (c,e) -> EXE_code (c, fe e)
  | EXE_noreturn_code (c,e) -> EXE_noreturn_code (c, fe e)
  | EXE_comment _
  | EXE_label _
  | EXE_goto _
    -> x
  | EXE_cgoto e -> EXE_cgoto (fe e)
  | EXE_ifcgoto (e1,e2) -> EXE_ifcgoto (fe e1, fe e2)
  | EXE_ifgoto (e,s) -> EXE_ifgoto (fe e,s)
  | EXE_call (a,b) -> EXE_call (fe a, fe b)
  | EXE_call_with_trap (a,b) -> EXE_call_with_trap (fe a, fe b)
  | EXE_jump (a,b) -> EXE_jump (fe a, fe b)
  | EXE_loop (s,e) -> EXE_loop (s, fe e)
  | EXE_svc _ -> x
  | EXE_fun_return e -> EXE_fun_return (fe e)
  | EXE_yield e -> EXE_yield (fe e)
  | EXE_proc_return -> x
  | EXE_halt _ -> x
  | EXE_trace _ -> x
  | EXE_nop _ -> x
  | EXE_init (name,e) -> EXE_init (name, fe e)
  | EXE_iinit ((name,idx),e) -> EXE_iinit ((name, fi idx), fe e)
  | EXE_assign (a,b) -> EXE_assign (fe a, fe b)
  | EXE_storeat (a,b) -> EXE_storeat (fe a, fe b)
  | EXE_assert e -> EXE_assert (fe e)
  | EXE_try  -> x
  | EXE_endtry -> x
  | EXE_catch (name,t) -> EXE_catch (name, ft t)
  | EXE_proc_return_from _ -> x


let rec iter_exe fi ft fe (x:exe_t):unit = match x with
  | EXE_begin_match_case
  | EXE_end_match_case -> ()

  | EXE_circuit cs ->  () 
  | EXE_type_error (x) -> iter_exe fi ft fe x
  | EXE_type_assert (x) -> iter_exe fi ft fe x
  | EXE_code (c,e) ->fe e
  | EXE_noreturn_code (c,e) -> fe e
  | EXE_comment _
  | EXE_label _
  | EXE_goto _
    ->() 
  | EXE_cgoto e -> fe e
  | EXE_ifcgoto (e1,e2) -> fe e1; fe e2
  | EXE_ifgoto (e,s) -> fe e
  | EXE_call (a,b) -> fe a; fe b
  | EXE_call_with_trap (a,b) ->fe a; fe b
  | EXE_jump (a,b) ->fe a; fe b
  | EXE_loop (s,e) -> fe e
  | EXE_svc _ -> ()
  | EXE_fun_return e -> fe e
  | EXE_yield e -> fe e
  | EXE_proc_return ->() 
  | EXE_halt _ -> ()
  | EXE_trace _ -> ()
  | EXE_nop _ -> ()
  | EXE_init (name,e) -> fe e
  | EXE_iinit ((name,idx),e) -> fi idx; fe e
  | EXE_assign (a,b) -> fe a; fe b
  | EXE_storeat (a,b) -> fe a; fe b
  | EXE_assert e -> fe e
  | EXE_try  -> ()
  | EXE_endtry -> ()
  | EXE_catch (name,t) -> ft t
  | EXE_proc_return_from _ -> ()





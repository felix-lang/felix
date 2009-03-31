open Flx_ast
open Flx_types
open List
open Flx_typing

let rec list_of_n_things thing lst n =
  if n = 0 then lst
  else list_of_n_things thing (thing::lst) (n-1)

let map_type f (t:typecode_t):typecode_t = match t with
  | `AST_name (sr,name,ts) -> `AST_name (sr,name, map f ts)
  | `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,name,map f ts))
  | `AST_suffix (sr,(qn,t)) -> `AST_suffix (sr,(qn, f t))

  | `AST_typed_case (sr,i,t) -> `AST_typed_case (sr,i, f t)
  | `TYP_tuple ts -> `TYP_tuple (map f ts)
  | `TYP_record ts -> `TYP_record (map (fun (s,t) -> s,f t) ts)
  | `TYP_variant ts -> `TYP_variant (map (fun (s,t) -> s,f t) ts)
  | `TYP_isin (a,b) -> `TYP_isin (f a, f b)

  (* we have to do this, so that a large unitsume
     can be specified without overflowing the compiler
     storage
  *)
  | `TYP_unitsum k ->
    if k>0 then
      let mapped_unit = f (`TYP_tuple []) in
      match mapped_unit with
      | `TYP_tuple [] ->
        `TYP_unitsum k
      | _ -> `TYP_tuple ( list_of_n_things mapped_unit [] k)
    else `TYP_unitsum k

  (* here we don't need to go to a unitsum, since
     we have already used up storage
  *)
  | `TYP_sum ts -> `TYP_sum (map f ts)
  | `TYP_intersect ts -> `TYP_intersect (map f ts)
  | `TYP_function (a,b) -> `TYP_function (f a, f b)
  | `TYP_cfunction (a,b) -> `TYP_cfunction (f a, f b)
  | `TYP_pointer t -> `TYP_pointer (f t)
(*  | `TYP_lvalue t -> `TYP_lvalue (f t) *)
  | `TYP_array (t1, t2) -> `TYP_array (f t1, f t2)
  | `TYP_as (t,s) -> `TYP_as (f t,s)

  (* type sets *)
  | `TYP_typeset ts -> `TYP_typeset (map f ts)
  | `TYP_setintersection ts -> `TYP_setintersection (map f ts)
  | `TYP_setunion ts -> `TYP_setunion (map f ts)

  (* destructors *)
  | `TYP_dom t -> `TYP_dom (f t)
  | `TYP_dual t -> `TYP_dual (f t)
  | `TYP_cod t -> `TYP_cod (f t)
  | `TYP_proj (i,t) -> `TYP_proj (i, f t)
  | `TYP_case_arg (i,t) -> `TYP_case_arg (i, f t)
  | `TYP_case (t1,ls,t2) -> `TYP_case (f t1, ls, f t2)

  (*
  | `TYP_type_match (t,ps) ->
    let ps = map (fun (p,t) -> p, f t) ps in
    `TYP_type_match (f t, ps)
  *)
  | `TYP_type_match (t,ps) ->
    let ps = map (fun (p,t) -> f p, f t) ps in
    `TYP_type_match (f t, ps)

  (* meta constructors *)
  | `TYP_apply (a,b) -> `TYP_apply (f a, f b)
  | `TYP_typefun (ps, a, b) -> `TYP_typefun (ps, f a, f b)
  | `TYP_type_tuple ts -> `TYP_type_tuple (map f ts)
  | `TYP_lift t -> `TYP_lift (f t)


  (* invariant ..?? *)
  | `TYP_typeof _
  | `AST_callback _
  | `AST_case_tag _
  | `AST_index _
  | `AST_the _
  | `TYP_var _
  | `AST_patvar _
  | `AST_patany _

  (* absolute constants *)
  | `AST_void _
  | `TYP_ellipsis
  | `TYP_type
  | `TYP_none

    -> t


let map_expr f (e:expr_t):expr_t = match e with
  | `AST_patvar _
  | `AST_patany _
  | `AST_vsprintf _ -> e
  | `AST_interpolate _ -> e
  | `AST_map (sr,a,b) -> `AST_map (sr,f a, f b)
  | `AST_noexpand (sr,x) -> e (* DO NOT EXPAND .. HMM .. *)
  | `AST_name _ -> e
  | `AST_callback _ -> e
  | `AST_the _ -> e
  | `AST_index _ -> e
  | `AST_case_tag _ -> e
  | `AST_typed_case _ -> e
  | `AST_lookup (sr,(x,s,ts)) -> `AST_lookup (sr,(f x, s, ts))
  | `AST_apply (sr,(a,b)) -> `AST_apply (sr,(f a, f b))
  | `AST_tuple (sr,es) -> `AST_tuple (sr, map f es)
  | `AST_record (sr,es) -> `AST_record (sr, map (fun (s,e) -> s,f e) es)
  | `AST_variant (sr,(s,e)) -> `AST_variant (sr, (s,f e))
  | `AST_arrayof (sr, es) -> `AST_arrayof (sr, map f es)
  | `AST_coercion (sr, (x,t)) -> `AST_coercion (sr,(f x, t))
  | `AST_suffix _ -> e

  | `AST_record_type (sr,ts) -> e
  | `AST_variant_type (sr,ts) -> e
  | `AST_void sr -> e
  | `AST_ellipsis sr -> e
  | `AST_product (sr,es) -> `AST_product (sr, map f es)
  | `AST_sum (sr,es) -> `AST_sum (sr, map f es)
  | `AST_setunion (sr,es) -> `AST_setunion (sr, map f es)
  | `AST_setintersection (sr,es) -> `AST_setintersection (sr, map f es)
  | `AST_intersect (sr,es) -> `AST_intersect (sr, map f es)
  | `AST_isin (sr,(a,b)) -> `AST_isin (sr, (f a, f b))
  | `AST_orlist (sr,es) -> `AST_orlist (sr, map f es)
  | `AST_andlist (sr,es) -> `AST_andlist (sr, map f es)
  | `AST_arrow (sr,(a,b)) -> `AST_arrow (sr,(f a, f b))
  | `AST_longarrow (sr,(a,b)) -> `AST_longarrow (sr,(f a, f b))
  | `AST_superscript (sr,(a,b)) -> `AST_superscript (sr,(f a, f b))

  | `AST_literal _ -> e
  | `AST_deref (sr,x) -> `AST_deref (sr,f x)
  | `AST_ref (sr,x) -> `AST_ref (sr, f x)
  | `AST_likely (sr,x) -> `AST_likely (sr, f x)
  | `AST_unlikely (sr,x) -> `AST_unlikely (sr, f x)
  | `AST_new (sr,x) -> `AST_new (sr, f x)
  | `AST_lift (sr,x) -> `AST_lift (sr, f x)
  | `AST_dot (sr,(x,x2)) -> `AST_dot (sr,(f x,f x2))

  (* GIVE UP ON LAMBDAS FOR THE MOMENT .. NEEDS STATEMENT MAPPING TOO *)
  (* | `AST_lambda of range_srcref * (vs_list_t * params_t list * typecode_t * statement_t list) *)
  | `AST_lambda _ -> e

  | `AST_match_ctor (sr,(qn,x)) -> `AST_match_ctor (sr,(qn,f x))
  | `AST_match_case (sr,(j,x)) -> `AST_match_case (sr,(j, f x))

  | `AST_ctor_arg (sr,(qn,x)) -> `AST_ctor_arg (sr,(qn,f x))
  | `AST_case_arg (sr,(j,x)) -> `AST_case_arg (sr,(j, f x))
  | `AST_case_index (sr,x) -> `AST_case_index (sr,f x)

  | `AST_letin (sr,(pat,a,b)) -> `AST_letin (sr,(pat,f a, f b))

  | `AST_get_n (sr,(j,x)) -> `AST_get_n (sr,(j,f x))
  | `AST_get_named_variable (sr,(j,x)) -> `AST_get_named_variable (sr,(j,f x))
  | `AST_as (sr,(x,s)) -> `AST_as (sr,(f x, s))
  | `AST_match (sr,(a,pes)) ->
    `AST_match (sr, (f a, map (fun (pat,x) -> pat, f x) pes))

  | `AST_typeof (sr,x) -> `AST_typeof (sr,f x)
  | `AST_cond (sr,(a,b,c)) -> `AST_cond (sr, (f a, f b, f c))

  | `AST_expr _ -> e
  | `AST_type_match _ -> e
  | `AST_macro_ctor _ -> e
  | `AST_macro_statements _ -> e
  | `AST_case (sr,e1,ls,e2) -> `AST_case (sr,f e1, ls, f e2)
  | `AST_user_expr (sr,term,ts) -> e (* ouch! *)


let iter_expr f (e:expr_t) =
  f e;
  match e with
  | `AST_patvar _
  | `AST_patany _
  | `AST_vsprintf _
  | `AST_interpolate _
  | `AST_name _
  | `AST_callback _
  | `AST_the _
  | `AST_index _
  | `AST_case_tag _
  | `AST_typed_case _
  | `AST_record_type _
  | `AST_variant_type _
  | `AST_void _
  | `AST_ellipsis _
  | `AST_noexpand _
  | `AST_suffix _
  | `AST_literal _
  | `AST_lambda _
  | `AST_expr _
  | `AST_type_match _
  | `AST_macro_ctor _
  | `AST_macro_statements _
    -> ()

  | `AST_variant (_,(_,x))
  | `AST_typeof (_,x)
  | `AST_as (_,(x,_))
  | `AST_get_n (_,(_,x))
  | `AST_get_named_variable (_,(_,x))
  | `AST_ctor_arg (_,(_,x))
  | `AST_case_arg (_,(_,x))
  | `AST_case_index (_,x)
  | `AST_match_ctor (_,(_,x))
  | `AST_match_case (_,(_,x))
  | `AST_deref (_,x)
  | `AST_ref (_,x)
  | `AST_likely (_,x)
  | `AST_unlikely (_,x)
  | `AST_new (_,x)
(*  | `AST_lvalue (_,x) *)
  | `AST_lookup (_,(x,_,_))
  | `AST_coercion (_, (x,_))
  | `AST_lift (_,x)
    -> f x

  | `AST_case (_,a,_,b)
  | `AST_letin (_,(_,a,b))
  | `AST_dot (_,(a,b))
  | `AST_longarrow (_,(a,b))
  | `AST_superscript (_,(a,b))
  | `AST_arrow (_,(a,b))
  | `AST_map (_,a,b)
  | `AST_apply (_,(a,b))
  | `AST_isin (_,(a,b))
    -> f a; f b

  | `AST_tuple (_,es)
  | `AST_product (_,es)
  | `AST_sum (_,es)
  | `AST_setunion (_,es)
  | `AST_intersect (_,es)
  | `AST_setintersection (_,es)
  | `AST_orlist (_,es)
  | `AST_andlist (_,es)
  | `AST_arrayof (_, es) ->
    iter f es

  | `AST_record (sr,es) -> iter (fun (s,e) -> f e) es

  | `AST_match (sr,(a,pes)) ->
    f a; iter (fun (pat,x) -> f x) pes

  | `AST_cond (sr,(a,b,c)) -> f a; f b; f c
  | `AST_user_expr (sr,term,ts) -> ()

let scan_expr e =
  let ls = ref [] in
  let add x = ls := Flx_srcref.src_of_expr x :: !ls in
  iter_expr add e;
  Flx_list.uniq_list !ls

let all_units' ts =
  try
    iter (function
      | `BTYP_tuple [] -> ()
      | _ -> raise Not_found
    )
    ts;
    true
  with Not_found -> false

let map_b0type f = function
  | `BTYP_inst (i,ts) -> `BTYP_inst (i, map f ts)
  | `BTYP_tuple ts -> `BTYP_tuple (map f ts)
  | `BTYP_record ts -> `BTYP_record (map (fun (s,t) -> s,f t) ts)
  | `BTYP_variant ts -> `BTYP_variant (map (fun (s,t) -> s,f t) ts)

  | `BTYP_unitsum k ->
    if k>0 then
      let mapped_unit = f (`BTYP_tuple []) in
      match mapped_unit with
      | `BTYP_tuple [] ->
        `BTYP_unitsum k
      | _ -> `BTYP_tuple ( list_of_n_things mapped_unit [] k)
    else `BTYP_unitsum k

  | `BTYP_intersect ts -> `BTYP_intersect (map f ts)

  | `BTYP_sum ts ->
    let ts = map f ts in
    if all_units' ts then
      `BTYP_unitsum (length ts)
    else
      `BTYP_sum ts

  | `BTYP_function (a,b) -> `BTYP_function (f a, f b)
  | `BTYP_cfunction (a,b) -> `BTYP_cfunction (f a, f b)
  | `BTYP_pointer t->  `BTYP_pointer (f t)
(*  | `BTYP_lvalue t->  `BTYP_lvalue (f t) *)
  | `BTYP_array (t1,t2)->  `BTYP_array (f t1, f t2)
  | x -> x

let map_btype f = function
  | `BTYP_apply (a,b) -> `BTYP_apply (f a, f b)
  | `BTYP_typefun (its, a, b) ->
     `BTYP_typefun (map (fun (i,t) -> i, f t) its, f a , f b)
  | `BTYP_type_tuple ts -> `BTYP_type_tuple (map f ts)
  | `BTYP_type_match (t,ps) ->
    (* this may be wrong .. hard to know .. *)
    let g (tp,t) = {tp with pattern=f tp.pattern},f t in
    `BTYP_type_match (f t, map g ps)

  | `BTYP_typeset ts ->
    let g acc elt =
      (* SHOULD USE UNIFICATIION! *)
      let elt = f elt in
      if mem elt acc then acc else elt::acc
    in
    let ts = rev(fold_left g [] ts) in
    if length ts = 1 then hd ts else
    `BTYP_typeset ts

  | `BTYP_typesetunion ls -> `BTYP_typesetunion (map f ls)
  | `BTYP_typesetintersection ls -> `BTYP_typesetintersection (map f ls)

  | `BTYP_type i -> `BTYP_type i
  | x -> map_b0type f x

let iter_b0type f = function
  | `BTYP_inst (i,ts) -> iter f ts
  | `BTYP_tuple ts -> iter f ts
  | `BTYP_record ts -> iter (fun (s,t) -> f t) ts
  | `BTYP_variant ts -> iter (fun (s,t) -> f t) ts
  | `BTYP_unitsum k ->
    let unitrep = `BTYP_tuple [] in
    for i = 1 to k do f unitrep done

  | `BTYP_sum ts -> iter f ts
  | `BTYP_function (a,b) -> f a; f b
  | `BTYP_cfunction (a,b) -> f a; f b
  | `BTYP_pointer t->  f t
(*  | `BTYP_lvalue t->  f t *)
  | `BTYP_array (t1,t2)->  f t1; f t2
  | x -> ()

let iter_btype f = function
  | `BTYP_apply (a,b) -> f a; f b
  | `BTYP_typefun (its, a, b) ->
     iter (fun (i,t) -> f t) its; f a; f b
  | `BTYP_type_match (t,ps) ->
    let g (tp,t) = f tp.pattern; f t in
    f t;
    iter g ps

  | `BTYP_type_tuple ts -> iter f ts
  | `BTYP_typeset ts -> iter f ts
  | `BTYP_typesetunion ts -> iter f ts
  | `BTYP_typesetintersection ts -> iter f ts

  | x -> iter_b0type f x

(* type invariant mapping *)

(* this routine applies arguments HOFs to SUB components only, not
   to the actual argument. It isn't recursive, so the argument HOF
   can be.
*)
let flat_iter_tbexpr fi fe ft ((x,t) as e) =
  match x with
  | `BEXPR_deref e -> fe e
  | `BEXPR_ref (i,ts) -> fi i; iter ft ts
  | `BEXPR_likely e -> fe e
  | `BEXPR_unlikely e -> fe e
  | `BEXPR_new e -> fe e
  | `BEXPR_not e -> fe e

  | `BEXPR_apply (e1,e2) -> fe e1; fe e2

  | `BEXPR_apply_prim (i,ts,e2) -> fi i; iter ft ts; fe e2
  | `BEXPR_apply_direct (i,ts,e2) -> fi i; iter ft ts; fe e2
  | `BEXPR_apply_struct (i,ts,e2) -> fi i; iter ft ts; fe e2
  | `BEXPR_apply_stack (i,ts,e2) -> fi i; iter ft ts; fe e2
  | `BEXPR_tuple  es -> iter fe es
  | `BEXPR_record es -> iter (fun (s,e) -> fe e) es
  | `BEXPR_variant (s,e) -> fe e

  | `BEXPR_get_n (i,e) -> fe e
  | `BEXPR_get_named (i,e) -> fi i; fe e

  | `BEXPR_closure (i,ts) -> fi i; iter ft ts
  | `BEXPR_name (i,ts) -> fi i; iter ft ts
  | `BEXPR_case (i,t') -> ft t'
  | `BEXPR_match_case (i,e) -> fe e
  | `BEXPR_case_arg (i,e) -> fe e
  | `BEXPR_case_index e -> fe e

  | `BEXPR_literal x -> ft t
  | `BEXPR_expr (s,t1) -> ft t1
  | `BEXPR_range_check (e1,e2,e3) -> fe e1; fe e2; fe e3
  | `BEXPR_coerce (e,t) -> fe e; ft t

(* this is a self-recursing version of the above routine: the argument
   to this routine must NOT recursively apply itself!
*)
let rec iter_tbexpr fi fe ft ((x,t) as e) =
  fe e; ft t;
  let fe e = iter_tbexpr fi fe ft e in
  flat_iter_tbexpr fi fe ft e


let map_tbexpr fi fe ft e = match e with
  | `BEXPR_deref e,t -> `BEXPR_deref (fe e),ft t
  | `BEXPR_ref (i,ts),t -> `BEXPR_ref (fi i, map ft ts), ft t
  | `BEXPR_new e,t -> `BEXPR_new (fe e), ft t
  | `BEXPR_not e,t -> `BEXPR_not (fe e), ft t
  | `BEXPR_likely e,t -> `BEXPR_likely (fe e), ft t
  | `BEXPR_unlikely e,t -> `BEXPR_unlikely (fe e), ft t

  | `BEXPR_apply (e1,e2),t -> `BEXPR_apply (fe e1, fe e2), ft t

  | `BEXPR_apply_prim (i,ts,e2),t -> `BEXPR_apply_prim (fi i, map ft ts, fe e2),ft t
  | `BEXPR_apply_direct (i,ts,e2),t -> `BEXPR_apply_direct (fi i, map ft ts, fe e2),ft t
  | `BEXPR_apply_struct (i,ts,e2),t -> `BEXPR_apply_struct (fi i, map ft ts, fe e2),ft t
  | `BEXPR_apply_stack (i,ts,e2),t -> `BEXPR_apply_stack (fi i, map ft ts, fe e2),ft t

  | `BEXPR_tuple  es,t -> `BEXPR_tuple (map fe es),ft t
  | `BEXPR_record es,t -> `BEXPR_record (map (fun (s,e) -> s, fe e) es),ft t
  | `BEXPR_variant (s,e),t -> `BEXPR_variant (s, fe e),ft t

  | `BEXPR_get_n (i,e),t -> `BEXPR_get_n (i, fe e),ft t
  | `BEXPR_get_named (i,e),t -> `BEXPR_get_named (fi i, fe e),ft t

  | `BEXPR_closure (i,ts),t -> `BEXPR_closure (fi i, map ft ts),ft t
  | `BEXPR_name (i,ts),t -> `BEXPR_name (fi i, map ft ts), ft t
  | `BEXPR_case (i,t'),t -> `BEXPR_case (i, ft t'),ft t
  | `BEXPR_match_case (i,e),t -> `BEXPR_match_case (i, fe e),ft t
  | `BEXPR_case_arg (i,e),t -> `BEXPR_case_arg (i, fe e),ft t
  | `BEXPR_case_index e,t -> `BEXPR_case_index (fe e),ft t

  | `BEXPR_literal x,t -> `BEXPR_literal x, ft t
  | `BEXPR_expr (s,t1),t2 -> `BEXPR_expr (s, ft t1), ft t2
  | `BEXPR_range_check (e1,e2,e3),t -> `BEXPR_range_check (fe e1,fe e2, fe e3), ft t
  | `BEXPR_coerce (e,t'),t -> `BEXPR_coerce (fe e, ft t'), ft t

let iter_bexe fi fe ft fl fldef exe =
  match exe with
  | `BEXE_call_prim (sr,i,ts,e2)
  | `BEXE_call_stack (sr,i,ts,e2)
  | `BEXE_call_direct (sr,i,ts,e2)
  | `BEXE_jump_direct (sr,i,ts,e2)
    -> fi i; iter ft ts; fe e2

  | `BEXE_assign (sr,e1,e2)
  | `BEXE_call (sr,e1,e2)
  | `BEXE_jump (sr,e1,e2)
    -> fe e1; fe e2

  | `BEXE_loop (sr,i,e)
    -> fi i; fe e

  | `BEXE_ifgoto (sr,e,lab)
    -> fe e; fl lab

  | `BEXE_label (sr,lab)
    -> fldef lab

  | `BEXE_goto (sr,lab)
    -> fl lab

  | `BEXE_fun_return (sr,e)
    -> fe e

  | `BEXE_yield (sr,e)
    -> fe e

  | `BEXE_axiom_check (_,e)
    -> fe e

  | `BEXE_assert2 (_,_,e1,e2)
    -> (match e1 with Some e -> fe e | None->()); fe e2

  | `BEXE_assert (_,e)
    -> fe e

  | `BEXE_init (sr,i,e)
    -> fi i; fe e

  | `BEXE_svc (sr,i)
    -> fi i

  | `BEXE_halt _
  | `BEXE_trace _
  | `BEXE_code _
  | `BEXE_nonreturn_code _
  | `BEXE_proc_return _
  | `BEXE_comment _
  | `BEXE_nop _
  | `BEXE_begin
  | `BEXE_end
    -> ()


let map_bexe fi fe ft fl fldef (exe:bexe_t):bexe_t =
  match exe with
  | `BEXE_call_prim (sr,i,ts,e2)  ->
    `BEXE_call_prim (sr,fi i,map ft ts, fe e2)

  | `BEXE_call_stack (sr,i,ts,e2) ->
    `BEXE_call_stack (sr,fi i, map ft ts, fe e2)

  | `BEXE_call_direct (sr,i,ts,e2) ->
    `BEXE_call_direct (sr,fi i,map ft ts,fe e2)

  | `BEXE_jump_direct (sr,i,ts,e2) ->
    `BEXE_jump_direct (sr,fi i,map ft ts,fe e2)

  | `BEXE_assign (sr,e1,e2) ->
    `BEXE_assign (sr,fe e1,fe e2)

  | `BEXE_call (sr,e1,e2) ->
    `BEXE_call (sr,fe e1, fe e2)

  | `BEXE_jump (sr,e1,e2) ->
    `BEXE_jump (sr,fe e1, fe e2)

  | `BEXE_loop (sr,i,e) ->
    `BEXE_loop (sr,fi i,fe e)

  | `BEXE_ifgoto (sr,e,lab)  ->
    `BEXE_ifgoto (sr,fe e,fl lab)

  | `BEXE_label (sr,lab) ->
    `BEXE_label (sr,fldef lab)

  | `BEXE_goto (sr,lab) ->
    `BEXE_goto (sr,fl lab)

  | `BEXE_fun_return (sr,e) ->
    `BEXE_fun_return (sr,fe e)

  | `BEXE_yield (sr,e) ->
    `BEXE_yield (sr,fe e)

  | `BEXE_assert (sr,e) ->
    `BEXE_assert (sr, fe e)

  | `BEXE_assert2 (sr,sr2,e1, e2) ->
     let e1 = match e1 with Some e1 -> Some (fe e1) | None -> None in
    `BEXE_assert2 (sr, sr2,e1, fe e2)

  | `BEXE_axiom_check (sr,e) ->
    `BEXE_axiom_check (sr, fe e)

  | `BEXE_init (sr,i,e) ->
    `BEXE_init (sr,fi i,fe e)

  | `BEXE_svc (sr,i) ->
    `BEXE_svc (sr,fi i)

  | `BEXE_halt _
  | `BEXE_trace _
  | `BEXE_code _
  | `BEXE_nonreturn_code _
  | `BEXE_proc_return _
  | `BEXE_comment _
  | `BEXE_nop _
  | `BEXE_begin
  | `BEXE_end
    -> exe

let ident x = x
let reduce_tbexpr bbdfns e =
  let rec aux e =
    match map_tbexpr ident aux ident e with
    | `BEXPR_apply((`BEXPR_closure (i,ts),_),a),t ->
      `BEXPR_apply_direct (i,ts,a),t

    | `BEXPR_get_n (n,((`BEXPR_tuple ls),_)),_ ->
      List.nth ls n

    | `BEXPR_deref (`BEXPR_ref (i,ts),_),t ->
      `BEXPR_name (i,ts),t

    | x -> x
  in aux e

let reduce_bexe bbdfns exe =
  match map_bexe ident (reduce_tbexpr bbdfns) ident ident ident exe with
  | `BEXE_call (sr,(`BEXPR_closure (i,ts),_),a) ->
    `BEXE_call_direct (sr,i,ts,a)
  | x -> x

let rec reduce_type t =
  match map_btype reduce_type t with
  | `BTYP_record ts ->
    begin match ts with
    | [] -> `BTYP_tuple []
    | _ ->
     let rcmp (s1,_) (s2,_) = compare s1 s2 in
     let ts = sort compare ts in
     let ss,ts = split ts in
     let ts = combine ss (map reduce_type ts) in
     `BTYP_record ts
    end
  | `BTYP_variant ts ->
    begin match ts with
    | [] -> `BTYP_void
    | _ ->
     let rcmp (s1,_) (s2,_) = compare s1 s2 in
     let ts = sort compare ts in
     let ss,ts = split ts in
     let ts = combine ss (map reduce_type ts) in
     `BTYP_variant ts
    end
  | `BTYP_tuple ts -> typeoflist ts
  | `BTYP_array (t',`BTYP_unitsum 0) -> `BTYP_tuple []
  | `BTYP_array (t',`BTYP_unitsum 1) -> t'
  | t -> t

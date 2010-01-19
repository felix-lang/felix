open Flx_ast
open Flx_types
open Flx_typing

let ident x = x

let rec list_of_n_things thing lst n =
  if n = 0 then lst
  else list_of_n_things thing (thing::lst) (n-1)

let map_type f (t:typecode_t):typecode_t = match t with
  | TYP_name (sr,name,ts) -> TYP_name (sr, name, List.map f ts)
  | TYP_lookup (sr,(e,name,ts)) -> TYP_lookup (sr, (e, name, List.map f ts))
  | TYP_suffix (sr,(qn,t)) -> TYP_suffix (sr, (qn, f t))
  | TYP_typed_case (sr,i,t) -> TYP_typed_case (sr,i, f t)
  | TYP_tuple ts -> TYP_tuple (List.map f ts)
  | TYP_record ts -> TYP_record (List.map (fun (s,t) -> s,f t) ts)
  | TYP_variant ts -> TYP_variant (List.map (fun (s,t) -> s,f t) ts)
  | TYP_isin (a,b) -> TYP_isin (f a, f b)

  (* we have to do this, so that a large unitsume
     can be specified without overflowing the compiler
     storage
  *)
  | TYP_unitsum k ->
    if k>0 then
      let mapped_unit = f (TYP_tuple []) in
      match mapped_unit with
      | TYP_tuple [] ->
        TYP_unitsum k
      | _ -> TYP_tuple ( list_of_n_things mapped_unit [] k)
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

  (* destructors *)
  | TYP_dom t -> TYP_dom (f t)
  | TYP_dual t -> TYP_dual (f t)
  | TYP_cod t -> TYP_cod (f t)
  | TYP_proj (i,t) -> TYP_proj (i, f t)
  | TYP_case_arg (i,t) -> TYP_case_arg (i, f t)

  (*
  | TYP_type_match (t,ps) ->
    let ps = List.map (fun (p,t) -> p, f t) ps in
    TYP_type_match (f t, ps)
  *)
  | TYP_type_match (t,ps) ->
    let ps = List.map (fun (p,t) -> f p, f t) ps in
    TYP_type_match (f t, ps)

  (* meta constructors *)
  | TYP_apply (a,b) -> TYP_apply (f a, f b)
  | TYP_typefun (ps, a, b) -> TYP_typefun (ps, f a, f b)
  | TYP_type_tuple ts -> TYP_type_tuple (List.map f ts)


  (* invariant ..?? *)
  | TYP_typeof _
  | TYP_callback _
  | TYP_case_tag _
  | TYP_index _
  | TYP_the _
  | TYP_var _
  | TYP_patvar _
  | TYP_patany _

  (* absolute constants *)
  | TYP_void _
  | TYP_ellipsis
  | TYP_type
  | TYP_none
  -> t


let map_expr f (e:expr_t):expr_t = match e with
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_vsprintf _ -> e
  | EXPR_map (sr,a,b) -> EXPR_map (sr,f a, f b)
  | EXPR_noexpand (sr,x) -> e (* DO NOT EXPAND .. HMM .. *)
  | EXPR_name _ -> e
  | EXPR_callback _ -> e
  | EXPR_the _ -> e
  | EXPR_index _ -> e
  | EXPR_case_tag _ -> e
  | EXPR_typed_case _ -> e
  | EXPR_lookup (sr,(x,s,ts)) -> EXPR_lookup (sr,(f x, s, ts))
  | EXPR_apply (sr,(a,b)) -> EXPR_apply (sr,(f a, f b))
  | EXPR_tuple (sr,es) -> EXPR_tuple (sr, List.map f es)
  | EXPR_record (sr,es) -> EXPR_record (sr, List.map (fun (s,e) -> s,f e) es)
  | EXPR_variant (sr,(s,e)) -> EXPR_variant (sr, (s,f e))
  | EXPR_arrayof (sr, es) -> EXPR_arrayof (sr, List.map f es)
  | EXPR_coercion (sr, (x,t)) -> EXPR_coercion (sr,(f x, t))
  | EXPR_suffix _ -> e

  | EXPR_record_type (sr,ts) -> e
  | EXPR_variant_type (sr,ts) -> e
  | EXPR_void sr -> e
  | EXPR_ellipsis sr -> e
  | EXPR_product (sr,es) -> EXPR_product (sr, List.map f es)
  | EXPR_sum (sr,es) -> EXPR_sum (sr, List.map f es)
  | EXPR_setunion (sr,es) -> EXPR_setunion (sr, List.map f es)
  | EXPR_setintersection (sr,es) -> EXPR_setintersection (sr, List.map f es)
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
  | EXPR_dot (sr,(x,x2)) -> EXPR_dot (sr,(f x,f x2))

  (* GIVE UP ON LAMBDAS FOR THE MOMENT .. NEEDS STATEMENT MAPPING TOO *)
  (* | EXPR_lambda of Flx_srcref.t * (vs_list_t * params_t list * typecode_t * statement_t list) *)
  | EXPR_lambda _ -> e

  | EXPR_match_ctor (sr,(qn,x)) -> EXPR_match_ctor (sr,(qn,f x))
  | EXPR_match_case (sr,(j,x)) -> EXPR_match_case (sr,(j, f x))

  | EXPR_ctor_arg (sr,(qn,x)) -> EXPR_ctor_arg (sr,(qn,f x))
  | EXPR_case_arg (sr,(j,x)) -> EXPR_case_arg (sr,(j, f x))
  | EXPR_case_index (sr,x) -> EXPR_case_index (sr,f x)

  | EXPR_letin (sr,(pat,a,b)) -> EXPR_letin (sr,(pat,f a, f b))

  | EXPR_get_n (sr,(j,x)) -> EXPR_get_n (sr,(j,f x))
  | EXPR_get_named_variable (sr,(j,x)) -> EXPR_get_named_variable (sr,(j,f x))
  | EXPR_as (sr,(x,s)) -> EXPR_as (sr,(f x, s))
  | EXPR_match (sr,(a,pes)) ->
    EXPR_match (sr, (f a, List.map (fun (pat,x) -> pat, f x) pes))

  | EXPR_typeof (sr,x) -> EXPR_typeof (sr,f x)
  | EXPR_cond (sr,(a,b,c)) -> EXPR_cond (sr, (f a, f b, f c))

  | EXPR_expr _ -> e
  | EXPR_type_match _ -> e
  | EXPR_macro_ctor _ -> e
  | EXPR_macro_statements _ -> e
  | EXPR_user_expr (sr,term,ts) -> e (* ouch! *)


let iter_expr f (e:expr_t) =
  f e;
  match e with
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_vsprintf _
  | EXPR_name _
  | EXPR_callback _
  | EXPR_the _
  | EXPR_index _
  | EXPR_case_tag _
  | EXPR_typed_case _
  | EXPR_record_type _
  | EXPR_variant_type _
  | EXPR_void _
  | EXPR_ellipsis _
  | EXPR_noexpand _
  | EXPR_suffix _
  | EXPR_literal _
  | EXPR_lambda _
  | EXPR_expr _
  | EXPR_type_match _
  | EXPR_macro_ctor _
  | EXPR_macro_statements _
    -> ()

  | EXPR_variant (_,(_,x))
  | EXPR_typeof (_,x)
  | EXPR_as (_,(x,_))
  | EXPR_get_n (_,(_,x))
  | EXPR_get_named_variable (_,(_,x))
  | EXPR_ctor_arg (_,(_,x))
  | EXPR_case_arg (_,(_,x))
  | EXPR_case_index (_,x)
  | EXPR_match_ctor (_,(_,x))
  | EXPR_match_case (_,(_,x))
  | EXPR_deref (_,x)
  | EXPR_ref (_,x)
  | EXPR_likely (_,x)
  | EXPR_unlikely (_,x)
  | EXPR_new (_,x)
  | EXPR_lookup (_,(x,_,_))
  | EXPR_coercion (_, (x,_))
    -> f x

  | EXPR_letin (_,(_,a,b))
  | EXPR_dot (_,(a,b))
  | EXPR_longarrow (_,(a,b))
  | EXPR_superscript (_,(a,b))
  | EXPR_arrow (_,(a,b))
  | EXPR_map (_,a,b)
  | EXPR_apply (_,(a,b))
  | EXPR_isin (_,(a,b))
    -> f a; f b

  | EXPR_tuple (_,es)
  | EXPR_product (_,es)
  | EXPR_sum (_,es)
  | EXPR_setunion (_,es)
  | EXPR_intersect (_,es)
  | EXPR_setintersection (_,es)
  | EXPR_orlist (_,es)
  | EXPR_andlist (_,es)
  | EXPR_arrayof (_, es) ->
    List.iter f es

  | EXPR_record (sr,es) -> List.iter (fun (s,e) -> f e) es

  | EXPR_match (sr,(a,pes)) ->
    f a; List.iter (fun (pat,x) -> f x) pes

  | EXPR_cond (sr,(a,b,c)) -> f a; f b; f c
  | EXPR_user_expr (sr,term,ts) -> ()

let scan_expr e =
  let ls = ref [] in
  let add x = ls := Flx_ast.src_of_expr x :: !ls in
  iter_expr add e;
  Flx_list.uniq_list !ls

let all_units' ts =
  try
    List.iter (function
      | BTYP_tuple [] -> ()
      | _ -> raise Not_found
    )
    ts;
    true
  with Not_found -> false

let map_btype f = function
  | BTYP_type_apply (a, b) -> btyp_type_apply (f a, f b)
  | BTYP_type_function (its, a, b) ->
      btyp_type_function (List.map (fun (i,t) -> i, f t) its, f a, f b)
  | BTYP_type_tuple ts -> btyp_type_tuple (List.map f ts)
  | BTYP_type_match (t,ps) ->
    (* this may be wrong .. hard to know .. *)
    let g (tp,t) = {tp with pattern=f tp.pattern},f t in
    btyp_type_match (f t, List.map g ps)

  | BTYP_type_set ts ->
    let g acc elt =
      (* SHOULD USE UNIFICATIION! *)
      let elt = f elt in
      if List.mem elt acc then acc else elt::acc
    in
    let ts = List.rev (List.fold_left g [] ts) in
    if List.length ts = 1 then List.hd ts else
    btyp_type_set ts

  | BTYP_type_set_union ls -> btyp_type_set_union (List.map f ls)
  | BTYP_type_set_intersection ls -> btyp_type_set_intersection (List.map f ls)

  | BTYP_type i -> btyp_type i

  | BTYP_inst (i,ts) -> btyp_inst (i, List.map f ts)
  | BTYP_tuple ts -> btyp_tuple (List.map f ts)
  | BTYP_record ts -> btyp_record (List.map (fun (s,t) -> s,f t) ts)
  | BTYP_variant ts -> btyp_variant (List.map (fun (s,t) -> s,f t) ts)

  | BTYP_unitsum k ->
    if k > 0 then
      let mapped_unit = f (btyp_tuple []) in
      match mapped_unit with
      | BTYP_tuple [] ->
        btyp_unitsum k
      | _ -> btyp_tuple (list_of_n_things mapped_unit [] k)
    else btyp_unitsum k

  | BTYP_intersect ts -> btyp_intersect (List.map f ts)

  | BTYP_sum ts ->
    let ts = List.map f ts in
    if all_units' ts then
      btyp_unitsum (List.length ts)
    else
      btyp_sum ts

  | BTYP_function (a,b) -> btyp_function (f a, f b)
  | BTYP_cfunction (a,b) -> btyp_cfunction (f a, f b)
  | BTYP_pointer t -> btyp_pointer (f t)
  | BTYP_array (t1,t2) -> btyp_array (f t1, f t2)
  | x -> x

let iter_btype f = function
  | BTYP_type_apply (a,b) -> f a; f b
  | BTYP_type_function (its, a, b) ->
     List.iter (fun (i,t) -> f t) its; f a; f b
  | BTYP_type_match (t,ps) ->
    let g (tp,t) = f tp.pattern; f t in
    f t;
    List.iter g ps

  | BTYP_type_tuple ts -> List.iter f ts
  | BTYP_type_set ts -> List.iter f ts
  | BTYP_type_set_union ts -> List.iter f ts
  | BTYP_type_set_intersection ts -> List.iter f ts

  | BTYP_inst (i,ts) -> List.iter f ts
  | BTYP_tuple ts -> List.iter f ts
  | BTYP_record ts -> List.iter (fun (s,t) -> f t) ts
  | BTYP_variant ts -> List.iter (fun (s,t) -> f t) ts
  | BTYP_unitsum k ->
    let unitrep = btyp_tuple [] in
    for i = 1 to k do f unitrep done

  | BTYP_sum ts -> List.iter f ts
  | BTYP_function (a,b) -> f a; f b
  | BTYP_cfunction (a,b) -> f a; f b
  | BTYP_pointer t->  f t
  | BTYP_array (t1,t2)->  f t1; f t2
  | x -> ()

(* type invariant mapping *)

(* this routine applies arguments HOFs to SUB components only, not
   to the actual argument. It isn't recursive, so the argument HOF
   can be.
*)
let flat_iter_tbexpr fi fe ft ((x,t) as e) =
  match x with
  | BEXPR_deref e -> fe e
  | BEXPR_ref (i,ts) -> fi i; List.iter ft ts
  | BEXPR_likely e -> fe e
  | BEXPR_unlikely e -> fe e
  | BEXPR_address e -> fe e
  | BEXPR_new e -> fe e

  | BEXPR_apply (e1,e2) -> fe e1; fe e2

  | BEXPR_apply_prim (i,ts,e2) -> fi i; List.iter ft ts; fe e2
  | BEXPR_apply_direct (i,ts,e2) -> fi i; List.iter ft ts; fe e2
  | BEXPR_apply_struct (i,ts,e2) -> fi i; List.iter ft ts; fe e2
  | BEXPR_apply_stack (i,ts,e2) -> fi i; List.iter ft ts; fe e2
  | BEXPR_tuple  es -> List.iter fe es
  | BEXPR_record es -> List.iter (fun (s,e) -> fe e) es
  | BEXPR_variant (s,e) -> fe e

  | BEXPR_get_n (i,e) -> fe e

  | BEXPR_closure (i,ts) -> fi i; List.iter ft ts
  | BEXPR_name (i,ts) -> fi i; List.iter ft ts
  | BEXPR_case (i,t') -> ft t'
  | BEXPR_match_case (i,e) -> fe e
  | BEXPR_case_arg (i,e) -> fe e
  | BEXPR_case_index e -> fe e

  | BEXPR_literal x -> ft t
  | BEXPR_expr (s,t1) -> ft t1
  | BEXPR_range_check (e1,e2,e3) -> fe e1; fe e2; fe e3
  | BEXPR_coerce (e,t) -> fe e; ft t

(* this is a self-recursing version of the above routine: the argument
   to this routine must NOT recursively apply itself!
*)
let rec iter_tbexpr fi fe ft ((x,t) as e) =
  fe e; ft t;
  let fe e = iter_tbexpr fi fe ft e in
  flat_iter_tbexpr fi fe ft e


let map_tbexpr fi fe ft e = match e with
  | BEXPR_deref e,t -> BEXPR_deref (fe e),ft t
  | BEXPR_ref (i,ts),t -> BEXPR_ref (fi i, List.map ft ts), ft t
  | BEXPR_new e,t -> BEXPR_new (fe e), ft t
  | BEXPR_address e,t -> BEXPR_address (fe e), ft t
  | BEXPR_likely e,t -> BEXPR_likely (fe e), ft t
  | BEXPR_unlikely e,t -> BEXPR_unlikely (fe e), ft t

  | BEXPR_apply (e1,e2),t -> BEXPR_apply (fe e1, fe e2), ft t

  | BEXPR_apply_prim (i,ts,e2),t -> BEXPR_apply_prim (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_direct (i,ts,e2),t -> BEXPR_apply_direct (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_struct (i,ts,e2),t -> BEXPR_apply_struct (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_stack (i,ts,e2),t -> BEXPR_apply_stack (fi i, List.map ft ts, fe e2),ft t

  | BEXPR_tuple  es,t -> BEXPR_tuple (List.map fe es),ft t
  | BEXPR_record es,t -> BEXPR_record (List.map (fun (s,e) -> s, fe e) es),ft t
  | BEXPR_variant (s,e),t -> BEXPR_variant (s, fe e),ft t

  | BEXPR_get_n (i,e),t -> BEXPR_get_n (i, fe e),ft t

  | BEXPR_closure (i,ts),t -> BEXPR_closure (fi i, List.map ft ts),ft t
  | BEXPR_name (i,ts),t -> BEXPR_name (fi i, List.map ft ts), ft t
  | BEXPR_case (i,t'),t -> BEXPR_case (i, ft t'),ft t
  | BEXPR_match_case (i,e),t -> BEXPR_match_case (i, fe e),ft t
  | BEXPR_case_arg (i,e),t -> BEXPR_case_arg (i, fe e),ft t
  | BEXPR_case_index e,t -> BEXPR_case_index (fe e),ft t

  | BEXPR_literal x,t -> BEXPR_literal x, ft t
  | BEXPR_expr (s,t1),t2 -> BEXPR_expr (s, ft t1), ft t2
  | BEXPR_range_check (e1,e2,e3),t -> BEXPR_range_check (fe e1,fe e2, fe e3), ft t
  | BEXPR_coerce (e,t'),t -> BEXPR_coerce (fe e, ft t'), ft t

let iter_bexe fi fe ft fl fldef exe =
  match exe with
  | BEXE_call_prim (sr,i,ts,e2)
  | BEXE_call_stack (sr,i,ts,e2)
  | BEXE_call_direct (sr,i,ts,e2)
  | BEXE_jump_direct (sr,i,ts,e2)
    -> fi i; List.iter ft ts; fe e2

  | BEXE_assign (sr,e1,e2)
  | BEXE_call (sr,e1,e2)
  | BEXE_jump (sr,e1,e2)
    -> fe e1; fe e2

  | BEXE_ifgoto (sr,e,lab)
    -> fe e; fl lab

  | BEXE_label (sr,lab)
    -> fldef lab

  | BEXE_goto (sr,lab)
    -> fl lab

  | BEXE_fun_return (sr,e)
    -> fe e

  | BEXE_yield (sr,e)
    -> fe e

  | BEXE_axiom_check (_,e)
    -> fe e

  | BEXE_assert2 (_,_,e1,e2)
    -> (match e1 with Some e -> fe e | None->()); fe e2

  | BEXE_assert (_,e)
    -> fe e

  | BEXE_init (sr,i,e)
    -> fi i; fe e

  | BEXE_svc (sr,i)
    -> fi i

  | BEXE_halt _
  | BEXE_trace _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_begin
  | BEXE_end
    -> ()

let map_bexe fi fe ft fl fldef (exe:bexe_t):bexe_t =
  match exe with
  | BEXE_call_prim (sr,i,ts,e2)  ->
    BEXE_call_prim (sr,fi i,List.map ft ts, fe e2)

  | BEXE_call_stack (sr,i,ts,e2) ->
    BEXE_call_stack (sr,fi i, List.map ft ts, fe e2)

  | BEXE_call_direct (sr,i,ts,e2) ->
    BEXE_call_direct (sr,fi i,List.map ft ts,fe e2)

  | BEXE_jump_direct (sr,i,ts,e2) ->
    BEXE_jump_direct (sr,fi i,List.map ft ts,fe e2)

  | BEXE_assign (sr,e1,e2) ->
    BEXE_assign (sr,fe e1,fe e2)

  | BEXE_call (sr,e1,e2) ->
    BEXE_call (sr,fe e1, fe e2)

  | BEXE_jump (sr,e1,e2) ->
    BEXE_jump (sr,fe e1, fe e2)

  | BEXE_ifgoto (sr,e,lab)  ->
    BEXE_ifgoto (sr,fe e,fl lab)

  | BEXE_label (sr,lab) ->
    BEXE_label (sr,fldef lab)

  | BEXE_goto (sr,lab) ->
    BEXE_goto (sr,fl lab)

  | BEXE_fun_return (sr,e) ->
    BEXE_fun_return (sr,fe e)

  | BEXE_yield (sr,e) ->
    BEXE_yield (sr,fe e)

  | BEXE_assert (sr,e) ->
    BEXE_assert (sr, fe e)

  | BEXE_assert2 (sr,sr2,e1, e2) ->
     let e1 = match e1 with Some e1 -> Some (fe e1) | None -> None in
    BEXE_assert2 (sr, sr2,e1, fe e2)

  | BEXE_axiom_check (sr,e) ->
    BEXE_axiom_check (sr, fe e)

  | BEXE_init (sr,i,e) ->
    BEXE_init (sr,fi i,fe e)

  | BEXE_svc (sr,i) ->
    BEXE_svc (sr,fi i)

  | BEXE_halt _
  | BEXE_trace _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_begin
  | BEXE_end
    -> exe

let reduce_tbexpr e =
  let rec aux e =
    match map_tbexpr ident aux ident e with
    | BEXPR_apply((BEXPR_closure (i,ts),_),a),t ->
      BEXPR_apply_direct (i,ts,a),t

    | BEXPR_get_n (n,((BEXPR_tuple ls),_)),_ ->
      List.nth ls n

    | BEXPR_deref (BEXPR_ref (i,ts),_),t ->
      BEXPR_name (i,ts),t

    | BEXPR_deref (BEXPR_address (e,t),_),_ -> (e,t)
    | BEXPR_address (BEXPR_deref (e,t),_),_ -> (e,t)

    | x -> x
  in aux e

let reduce_bexe exe =
  match map_bexe ident reduce_tbexpr ident ident ident exe with
  | BEXE_call (sr,(BEXPR_closure (i,ts),_),a) ->
    BEXE_call_direct (sr,i,ts,a)
  | x -> x

let rec reduce_type t =
  match map_btype reduce_type t with
  | BTYP_record ts ->
    begin match ts with
    | [] -> btyp_tuple []
    | _ ->
     let rcmp (s1,_) (s2,_) = compare s1 s2 in
     let ts = List.sort compare ts in
     let ss,ts = List.split ts in
     let ts = List.combine ss (List.map reduce_type ts) in
     btyp_record ts
    end
  | BTYP_variant ts ->
    begin match ts with
    | [] -> btyp_void
    | _ ->
     let rcmp (s1,_) (s2,_) = compare s1 s2 in
     let ts = List.sort compare ts in
     let ss,ts = List.split ts in
     let ts = List.combine ss (List.map reduce_type ts) in
     btyp_variant ts
    end
  | BTYP_array (t',BTYP_unitsum 0) -> btyp_tuple []
  | BTYP_array (t',BTYP_unitsum 1) -> t'
  | t -> t

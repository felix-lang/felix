open Flx_types

(** Remaps a bound index by adding an offset to it. *)
let remap_bid offset bid = bid + offset

(** Remaps bound types by adding an offset to the bound index. *)
let rec remap_btype offset btype =
  let remap_bid = remap_bid offset in
  let remap_btype = remap_btype offset in
  match btype with
  | BTYP_inst (bid, ts) ->
      BTYP_inst (remap_bid bid, List.map remap_btype ts)

  | BTYP_tuple ts ->
      BTYP_tuple (List.map remap_btype ts)

  | BTYP_record ts ->
      BTYP_record (List.map (fun (n,t) -> n, remap_btype t) ts)

  | BTYP_unitsum i ->
      BTYP_unitsum i

  | BTYP_variant ts ->
      BTYP_variant (List.map (fun (n,t) -> n, remap_btype t) ts)

  | BTYP_sum ts ->
      BTYP_sum (List.map remap_btype ts)

  | BTYP_function (args, result) ->
      BTYP_function (remap_btype args, remap_btype result)

  | BTYP_cfunction (args, result) ->
      BTYP_cfunction (remap_btype args, remap_btype result)

  | BTYP_pointer t ->
      BTYP_pointer (remap_btype t)

  | BTYP_array (vt, it) ->
      BTYP_array (remap_btype vt, remap_btype it)

  | BTYP_void ->
      BTYP_void

  | BTYP_fix i ->
      BTYP_fix i

  | BTYP_intersect ts ->
      BTYP_intersect (List.map remap_btype ts)

  | BTYP_type_var (bid, t) ->
      BTYP_type_var (remap_bid bid, remap_btype t)

  | BTYP_type_apply (t1, t2) ->
      BTYP_type_apply (remap_btype t1, remap_btype t2)

  | BTYP_type_function (args, result, body) ->
      let args = List.map (fun (i, t) -> remap_bid i, remap_btype t) args in
      BTYP_type_function (args, remap_btype result, remap_btype body)

  | BTYP_type i ->
      BTYP_type i

  | BTYP_type_tuple ts ->
      BTYP_type_tuple (List.map remap_btype ts)

  | BTYP_type_match (t, ps) ->
      let ps =
        List.map begin fun (pat, t) ->
          { pattern = remap_btype pat.pattern;
            pattern_vars = BidSet.fold
              (fun i bs -> BidSet.add (remap_bid i) bs)
              pat.pattern_vars
              BidSet.empty;
            assignments = List.map
              (fun (i, t) -> i, remap_btype t)
              pat.assignments }, remap_btype t
        end ps
      in
      BTYP_type_match (remap_btype t, ps)

  | BTYP_type_set ts ->
      BTYP_type_set (List.map remap_btype ts)

  | BTYP_type_set_union ts ->
      BTYP_type_set_union (List.map remap_btype ts)

  | BTYP_type_set_intersection ts ->
      BTYP_type_set_intersection (List.map remap_btype ts)


(** Remap bound interfaces by adding an offset to the bound index. *)
let remap_biface offset biface =
  match biface with
  | BIFACE_export_fun (sr, bid, name) ->
      BIFACE_export_fun (sr, remap_bid offset bid, name)

  | BIFACE_export_python_fun (sr, bid, name) ->
      BIFACE_export_python_fun (sr, remap_bid offset bid, name)

  | BIFACE_export_type (sr, btype, name) ->
      BIFACE_export_type (sr, remap_btype offset btype, name)


(** Remap bound types by adding an offset to the bound index. *)
let rec remap_tbexpr offset (bexpr, btype) =
  let remap_bid = remap_bid offset in
  let remap_btype = remap_btype offset in
  let remap_tbexpr = remap_tbexpr offset in
  begin match bexpr with
  | BEXPR_deref e ->
      BEXPR_deref (remap_tbexpr e)

  | BEXPR_name (i, ts) ->
      BEXPR_name (remap_bid i, List.map remap_btype ts)

  | BEXPR_ref (i, ts) ->
      BEXPR_ref (remap_bid i, List.map remap_btype ts)

  | BEXPR_likely e ->
      BEXPR_likely (remap_tbexpr e)

  | BEXPR_unlikely e ->
      BEXPR_unlikely (remap_tbexpr e)

  | BEXPR_address e ->
      BEXPR_address (remap_tbexpr e)

  | BEXPR_new e ->
      BEXPR_new (remap_tbexpr e)

  | BEXPR_literal l ->
      BEXPR_literal l

  | BEXPR_apply (fn, args) ->
      BEXPR_apply (remap_tbexpr fn, remap_tbexpr args)

  | BEXPR_apply_prim (i, ts, args) ->
      let ts = List.map remap_btype ts in
      let args = remap_tbexpr args in
      BEXPR_apply_prim (remap_bid i, ts, args)

  | BEXPR_apply_direct (i, ts, args) ->
      let ts = List.map remap_btype ts in
      let args = remap_tbexpr args in
      BEXPR_apply_direct (remap_bid i, ts, args)

  | BEXPR_apply_stack (i, ts, args) ->
      let ts = List.map remap_btype ts in
      let args = remap_tbexpr args in
      BEXPR_apply_stack (remap_bid i, ts, args)

  | BEXPR_apply_struct (i, ts, args) ->
      let ts = List.map remap_btype ts in
      let args = remap_tbexpr args in
      BEXPR_apply_struct (remap_bid i, ts, args)

  | BEXPR_tuple es ->
      BEXPR_tuple (List.map remap_tbexpr es)

  | BEXPR_record es ->
      BEXPR_record (List.map (fun (n,e) -> n, remap_tbexpr e) es)

  | BEXPR_variant (n, e) ->
      BEXPR_variant (n, remap_tbexpr e)

  | BEXPR_get_n (n, e) ->
      BEXPR_get_n (n, remap_tbexpr e)

  | BEXPR_closure (i, ts) ->
      BEXPR_closure (remap_bid i, List.map remap_btype ts)

  | BEXPR_case (i, t) ->
      BEXPR_case (i, remap_btype t)

  | BEXPR_match_case (i, e) ->
      BEXPR_match_case (i, remap_tbexpr e)

  | BEXPR_case_arg (i, e) ->
      BEXPR_case_arg (i, remap_tbexpr e)

  | BEXPR_case_index e ->
      BEXPR_case_index (remap_tbexpr e)

  | BEXPR_expr (n, t) ->
      BEXPR_expr (n, remap_btype t)

  | BEXPR_range_check (e1, e2, e3) ->
      BEXPR_range_check (remap_tbexpr e1, remap_tbexpr e2, remap_tbexpr e3)

  | BEXPR_coerce (e, t) ->
      BEXPR_coerce (remap_tbexpr e, remap_btype t)

  end, remap_btype btype


(** Remap bound exes by adding an offset to the bound index. *)
let remap_bexe offset bexe =
  let remap_bid = remap_bid offset in
  let remap_btype = remap_btype offset in
  let remap_tbexpr = remap_tbexpr offset in
  match bexe with
  | BEXE_label (sr, s) ->
      BEXE_label (sr, s)

  | BEXE_comment (sr, s) ->
      BEXE_comment (sr, s)

  | BEXE_halt (sr, s) ->
      BEXE_halt (sr, s)

  | BEXE_trace (sr, s1, s2) ->
      BEXE_trace (sr, s1, s2)

  | BEXE_goto (sr, s) ->
      BEXE_goto (sr, s)

  | BEXE_ifgoto (sr, e, s) ->
      BEXE_ifgoto (sr, remap_tbexpr e, s)

  | BEXE_call (sr, p, a) ->
      BEXE_call (sr, remap_tbexpr p, remap_tbexpr a)

  | BEXE_call_direct (sr, i, ts, a) ->
      let ts = List.map remap_btype ts in
      BEXE_call_direct (sr, remap_bid i, ts, remap_tbexpr a)

  | BEXE_call_stack (sr, i, ts, a) ->
      let ts = List.map remap_btype ts in
      BEXE_call_stack (sr, remap_bid i, ts, remap_tbexpr a)

  | BEXE_call_prim (sr, i, ts, a) ->
      let ts = List.map remap_btype ts in
      BEXE_call_prim (sr, remap_bid i, ts, remap_tbexpr a)

  | BEXE_jump (sr, p, a) ->
      BEXE_jump (sr, remap_tbexpr p, remap_tbexpr a)

  | BEXE_jump_direct (sr, i, ts, a) ->
      let ts = List.map remap_btype ts in
      BEXE_jump_direct (sr, remap_bid i, ts, remap_tbexpr a)

  | BEXE_svc (sr, v) ->
      BEXE_svc (sr, remap_bid v)

  | BEXE_fun_return (sr, e) ->
      BEXE_fun_return (sr, remap_tbexpr e)

  | BEXE_yield (sr, e) ->
      BEXE_yield (sr, remap_tbexpr e)

  | BEXE_proc_return sr ->
      BEXE_proc_return sr

  | BEXE_nop (sr, s) ->
      BEXE_nop (sr, s)

  | BEXE_code (sr, code) ->
      BEXE_code (sr, code)

  | BEXE_nonreturn_code (sr, code) ->
      BEXE_nonreturn_code (sr, code)

  | BEXE_assign (sr, l, r) ->
      BEXE_assign (sr, remap_tbexpr l, remap_tbexpr r)

  | BEXE_init (sr, l, r) ->
      BEXE_init (sr, remap_bid l, remap_tbexpr r)

  | BEXE_begin ->
      BEXE_begin

  | BEXE_end ->
      BEXE_end

  | BEXE_assert (sr, e) ->
      BEXE_assert (sr, remap_tbexpr e)

  | BEXE_assert2 (sr1, sr2, e1, e2) ->
      let e1 = match e1 with None -> None | Some e1 -> Some (remap_tbexpr e1) in
      BEXE_assert2 (sr1, sr2, e1, remap_tbexpr e2)

  | BEXE_axiom_check (sr, e) ->
      BEXE_axiom_check (sr, remap_tbexpr e)


(** Remaps bound declarations by adding an offset to the bound index. *)
let remap_bbdcl offset bbdcl =
  let remap_bid = remap_bid offset in
  let remap_btype = remap_btype offset in
  let remap_tbexpr = remap_tbexpr offset in
  let remap_bexe = remap_bexe offset in
  let remap_bvs = List.map (fun (n, i) -> n, remap_bid i) in
  let remap_bparameter bpar =
    { bpar with
      pindex = remap_bid bpar.pindex;
      ptyp = remap_btype bpar.ptyp }
  in
  let remap_bparams (bparameters, e) =
    let e = match e with None -> None | Some t -> Some (remap_tbexpr t) in
    (List.map remap_bparameter bparameters), e
  in
  let remap_breqs breqs =
    List.map (fun (i, ts) -> remap_bid i, List.map remap_btype ts) breqs
  in
  match bbdcl with
  | BBDCL_module ->
      BBDCL_module

  | BBDCL_function (props, vs, ps, res, es) ->
      let vs = remap_bvs vs in
      let ps = remap_bparams ps in
      let res = remap_btype res in
      let es = List.map remap_bexe es in
      BBDCL_function (props, vs, ps, res, es)

  | BBDCL_procedure (props, vs, ps, es) ->
      let vs = remap_bvs vs in
      let ps = remap_bparams ps in
      let es = List.map remap_bexe es in
      BBDCL_procedure (props, vs, ps, es)

  | BBDCL_val (vs, ty) ->
      BBDCL_val (remap_bvs vs, remap_btype ty)

  | BBDCL_var (vs, ty) ->
      BBDCL_var (remap_bvs vs, remap_btype ty)

  | BBDCL_ref (vs, ty) ->
      BBDCL_ref (remap_bvs vs, remap_btype ty)

  | BBDCL_tmp (vs, ty) ->
      BBDCL_tmp (remap_bvs vs, remap_btype ty)

  | BBDCL_newtype (vs, ty) ->
      BBDCL_newtype (remap_bvs vs, remap_btype ty)

  | BBDCL_abs (vs, quals, code, reqs) ->
      let vs = remap_bvs vs in
      let quals =
        List.map begin function
          | `Bound_needs_shape t -> `Bound_needs_shape (remap_btype t)
          | qual -> qual
        end quals
      in
      let reqs = remap_breqs reqs in
      BBDCL_abs (vs, quals, code, reqs)

  | BBDCL_const (props, vs, ty, code, reqs) ->
      let vs = remap_bvs vs in
      let ty = remap_btype ty in
      let reqs = remap_breqs reqs in
      BBDCL_const (props, vs, ty, code, reqs)

  | BBDCL_fun (props, vs, ps, rt, code, reqs, prec) ->
      let vs = remap_bvs vs in
      let ps = List.map remap_btype ps in
      let rt = remap_btype rt in
      let reqs = remap_breqs reqs in
      BBDCL_fun (props, vs, ps, rt, code, reqs, prec)

  | BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec) ->
      let vs = remap_bvs vs in
      let ps_cf = List.map remap_btype ps_cf in
      let ps_c = List.map remap_btype ps_c in
      let rt = remap_btype rt in
      let reqs = remap_breqs reqs in
      BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec)

  | BBDCL_proc (props, vs, ps, code, reqs) ->
      let vs = remap_bvs vs in
      let ps = List.map remap_btype ps in
      let reqs = remap_breqs reqs in
      BBDCL_proc (props, vs, ps, code, reqs)

  | BBDCL_insert (vs, code, ikind, reqs) ->
      let vs = remap_bvs vs in
      let reqs = remap_breqs reqs in
      BBDCL_insert (vs, code, ikind, reqs)

  | BBDCL_union (vs, cs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,v,t) -> n,v,remap_btype t) cs in
      BBDCL_union (vs, cs)

  | BBDCL_struct (vs, cs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,t) -> n,remap_btype t) cs in
      BBDCL_struct (vs, cs)

  | BBDCL_cstruct (vs, cs) ->
      let vs = remap_bvs vs in
      let cs = List.map (fun (n,t) -> n,remap_btype t) cs in
      BBDCL_cstruct (vs, cs)

  | BBDCL_typeclass (props, vs) ->
      BBDCL_typeclass (props, remap_bvs vs)

  | BBDCL_instance (props, vs, cons, bid, ts) ->
      let vs = remap_bvs vs in
      let cons = remap_btype cons in
      let bid = remap_bid bid in
      let ts = List.map remap_btype ts in
      BBDCL_instance (props, vs, cons, bid, ts)

  | BBDCL_nonconst_ctor (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) ->
      let vs = remap_bvs vs in
      let uidx = remap_bid uidx in
      let ut = remap_btype ut in
      let ctor_argt = remap_btype ctor_argt in
      let evs = remap_bvs evs in
      let etraint = remap_btype etraint in
      BBDCL_nonconst_ctor (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint)


(** Remap symbols from an old bound symbol table to a new one by offsetting the
 * bound index by a constant amount. *)
let remap offset in_bsym_table out_bsym_table =
  Flx_bsym_table.iter begin fun bid bsym ->
    let bid = remap_bid offset bid in
    let parent =
      match bsym.Flx_bsym.parent with
      | None -> None
      | Some parent -> Some (remap_bid offset bid)
    in
    let bbdcl = remap_bbdcl offset bsym.Flx_bsym.bbdcl in
    Flx_bsym_table.add out_bsym_table bid { bsym with Flx_bsym.bbdcl=bbdcl }
  end in_bsym_table

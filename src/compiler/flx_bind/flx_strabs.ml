open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open List
open Flx_unify
open Flx_generic
open Flx_maps
open Flx_exceptions

type strabs_state_t = unit

let make_strabs_state () = ()

let check_inst bsym_table i ts =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_newtype (vs,t) -> tsubst vs ts t
  | _ -> btyp_inst (i,ts)

let fixtype bsym_table t =
  let chk i ts = check_inst bsym_table i ts in
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_inst (i,ts) ->
        let ts = map f_btype ts in
        chk i ts
    | x -> x
  in
  f_btype t

let fixexpr bsym_table e =
  let rec f_bexpr e =
    match Flx_bexpr.map ~f_btype:(fixtype bsym_table) ~f_bexpr e with
    | BEXPR_apply ( (BEXPR_closure(i,_),_),a),_
    | BEXPR_apply_direct (i,_,a),_
    | BEXPR_apply_prim (i,_,a),_
      when Flx_bsym_table.is_identity bsym_table i -> a
    | x -> x
  in
  f_bexpr e

let fixbexe bsym_table x =
  Flx_bexe.map ~f_btype:(fixtype bsym_table) ~f_bexpr:(fixexpr bsym_table) x

let fixbexes bsym_table bexes = map (fixbexe bsym_table) bexes

let fixps bsym_table (ps,traint) =
  List.map (fun p -> { p with ptyp=fixtype bsym_table p.ptyp }) ps,
  (
  match traint with
  | None -> None
  | Some t -> Some (fixexpr bsym_table t)
  )

let strabs_symbol state bsym_table index bsym =
  let ft t = fixtype bsym_table t in
  let fts ts = map (fixtype bsym_table) ts in
  let fe e = fixexpr bsym_table e in
  let fxs xs = fixbexes bsym_table xs in
  let fp bps = fixps bsym_table bps in

  let h bbdcl = Flx_bsym_table.update_bbdcl bsym_table index bbdcl in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_invalid ->
    assert false

  | BBDCL_module ->
    h (bbdcl_module ())

  | BBDCL_function (props, bvs, bps, ret, bexes) ->
    h (bbdcl_function (props, bvs, fp bps, ft ret, fxs bexes))

  | BBDCL_procedure (props, bvs, bps, bexes) ->
    h (bbdcl_procedure (props, bvs, fp bps, fxs bexes))

  | BBDCL_val (bvs, t, kind) ->
    h (bbdcl_val (bvs, ft t, kind))

  | BBDCL_newtype (bvs, t) ->
    (* Can't downgrade this newtype yet. *)
    ()

  | BBDCL_abs (bvs, btqs, c, breqs) ->
    h (bbdcl_abs (bvs, btqs, c, breqs))

  | BBDCL_const (props, bvs, t, c, breqs) ->
    h (bbdcl_const (props,  bvs, ft t, c, breqs))

  | BBDCL_fun (props, bvs, ts, t, c, breqs, prec) ->
    if c = CS_identity then
      (* Ignore identity functions. *)
      ()
    else
      h (bbdcl_fun (props, bvs, fts ts, ft t, c, breqs, prec))

  | BBDCL_callback (props, bvs, ts1, ts2, j, t, breqs, prec) ->
    h (bbdcl_callback (props, bvs, fts ts1, fts ts2, j, ft t, breqs, prec))

  | BBDCL_proc (props, bvs, ts, c, breqs) ->
    h (bbdcl_proc (props, bvs, fts ts, c, breqs))

  | BBDCL_insert (bvs, c, ikind, breqs) ->
    h (bbdcl_insert (bvs, c, ikind, breqs))

  | BBDCL_union (bvs, cts) ->
    let cts = map (fun (s,j,t) -> s,j,ft t) cts in
    h (bbdcl_union (bvs, cts))

  | BBDCL_struct (bvs, cts) ->
    let cts = map (fun (s,t) -> s,ft t) cts in
    h (bbdcl_struct (bvs, cts))

  | BBDCL_cstruct (bvs, cts) ->
    let cts = map (fun (s,t) -> s,ft t) cts in
    h (bbdcl_cstruct (bvs, cts))

  | BBDCL_typeclass (props, bvs) ->
    h (bbdcl_typeclass (props, bvs))

  | BBDCL_instance (props, bvs, t, j, ts) ->
    h (bbdcl_instance (props, bvs, ft t, j, fts ts))

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) ->
    h (bbdcl_nonconst_ctor (bvs, j, ft t1, k, ft t2, evs, ft etraint))

  | BBDCL_axiom ->
    h (bbdcl_axiom ())

  | BBDCL_lemma ->
    h (bbdcl_lemma ())

  | BBDCL_reduce ->
    h (bbdcl_reduce ())

let strabs state bsym_table =
  (* Copy the bsym_table since we're going to directly modify it. *)
  let bsym_table' = Flx_bsym_table.copy bsym_table in

  Flx_bsym_table.iter begin fun index symbol ->
    strabs_symbol state bsym_table index symbol
  end bsym_table'

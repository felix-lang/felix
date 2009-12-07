open Flx_util
open Flx_ast
open Flx_types
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
  | _ -> BTYP_inst (i,ts)

let fixtype bsym_table t =
  let chk i ts = check_inst bsym_table i ts in
  let rec aux t = match map_btype aux t with
  | BTYP_inst (i,ts) ->
    let ts = map aux ts in
    chk i ts
  | x -> x
  in aux t

let id x = x

let fixexpr bsym_table e : tbexpr_t =
  let rec aux e =
    match map_tbexpr id aux (fixtype bsym_table) e with
    | BEXPR_apply ( (BEXPR_closure(i,_),_),a),_
    | BEXPR_apply_direct (i,_,a),_
    | BEXPR_apply_prim (i,_,a),_
      when Flx_bsym_table.is_identity bsym_table i -> a
    | x -> x
  in aux e

let fixbexe bsym_table x =
  map_bexe id (fixexpr bsym_table) (fixtype bsym_table) id id x

let fixbexes bsym_table bexes = map (fixbexe bsym_table) bexes

let fixps bsym_table (ps,traint) =
  map
  (fun {pkind=i;pid=s; pindex=j; ptyp=t} ->
    {pkind=i; pid=s; pindex=j; ptyp=fixtype bsym_table t}
  )
  ps,
  (
  match traint with
  | None -> None
  | Some t -> Some (fixexpr bsym_table t)
  )

let strabs_symbol state bsym_table index (id,parent,sr,entry) =
  let ft t = fixtype bsym_table t in
  let fts ts = map (fixtype bsym_table) ts in
  let fe e = fixexpr bsym_table e in
  let fxs xs = fixbexes bsym_table xs in
  let fp bps = fixps bsym_table bps in

  let h x = Flx_bsym_table.add bsym_table index (id,parent,sr,x) in
  match entry with
  | BBDCL_function (props, bvs, bps, ret, bexes) ->
    h (BBDCL_function (props, bvs, fp bps, ft ret, fxs bexes))

  | BBDCL_procedure (props, bvs, bps, bexes) ->
    h (BBDCL_procedure (props, bvs, fp bps, fxs bexes))

  | BBDCL_val (bvs, t) ->
    h (BBDCL_val (bvs, ft t))

  | BBDCL_var (bvs, t) ->
    h (BBDCL_var (bvs, ft t))

  | BBDCL_ref (bvs, t) ->
    h (BBDCL_ref (bvs, ft t))

  | BBDCL_tmp (bvs, t) ->
    h (BBDCL_tmp (bvs, ft t))

  | BBDCL_newtype (bvs, t) ->
    (* Can't downgrade this newtype yet. *)
    ()

  | BBDCL_abs (bvs, btqs, c, breqs) ->
    h (BBDCL_abs (bvs, btqs, c, breqs))

  | BBDCL_const (props, bvs, t, c, breqs) ->
    h (BBDCL_const (props,  bvs, ft t, c, breqs))

  | BBDCL_fun (props, bvs, ts, t, c, breqs, prec) ->
    if c = CS_identity then
      (* Ignore identity functions. *)
      ()
    else
      h (BBDCL_fun (props, bvs, fts ts, ft t, c, breqs, prec))

  | BBDCL_callback (props, bvs, ts1, ts2, j, t, breqs, prec) ->
    h (BBDCL_callback (props, bvs, fts ts1, fts ts2, j, ft t, breqs, prec))

  | BBDCL_proc (props, bvs, ts, c, breqs) ->
    h (BBDCL_proc (props, bvs, fts ts, c, breqs))

  | BBDCL_insert (bvs, c, ikind, breqs) ->
    h (BBDCL_insert (bvs, c, ikind, breqs))

  | BBDCL_union (bvs, cts) ->
    let cts = map (fun (s,j,t) -> s,j,ft t) cts in
    h (BBDCL_union (bvs, cts))

  | BBDCL_struct (bvs, cts) ->
    let cts = map (fun (s,t) -> s,ft t) cts in
    h (BBDCL_struct (bvs, cts))

  | BBDCL_cstruct (bvs, cts) ->
    let cts = map (fun (s,t) -> s,ft t) cts in
    h (BBDCL_cstruct (bvs, cts))

  | BBDCL_typeclass (props, bvs) ->
    h (BBDCL_typeclass (props, bvs))

  | BBDCL_instance (props, bvs, t, j, ts) ->
    h (BBDCL_instance (props, bvs, ft t, j, fts ts))

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) ->
    h (BBDCL_nonconst_ctor (bvs, j, ft t1, k, ft t2, evs, ft etraint))

let strabs state bsym_table =
  (* Copy the bsym_table since we're going to directly modify it. *)
  let bsym_table' = Flx_bsym_table.copy bsym_table in

  Flx_bsym_table.iter begin fun index symbol ->
    strabs_symbol state bsym_table index symbol
  end bsym_table'

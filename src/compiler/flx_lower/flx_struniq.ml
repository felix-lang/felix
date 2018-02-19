
open Flx_util
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_btype_subst


let fixtype bsym_table t =
  let rec f_btype t =
    let t = Flx_btype.map ~f_btype t in
    match t with 
(* Remove uniqueness types *)
    | BTYP_uniq t -> t
(* downgrade read and write pointers to ordinary pointers *)
    | BTYP_rref t -> btyp_pointer t
    | BTYP_wref t -> btyp_pointer t
    | _ -> t
  in
  f_btype t

let fixqual bsym_table qual = 
  match qual with
  | `Bound_needs_shape t -> `Bound_needs_shape (fixtype bsym_table t)
  | x -> x

let fixquals bsym_table quals =
  map (fixqual bsym_table) quals

let fixreq bsym_table  (bid,ts) =
  bid, map (fixtype bsym_table) ts

let fixreqs bsym_table reqs = map (fixreq bsym_table) reqs

(* this analysis has to be top down not bottom up, so that
the solo union detectors work
*)
let rec fixexpr' bsym_table e =
  let f_btype t = fixtype bsym_table t in 
  let f_bexpr e = fixexpr' bsym_table e in
  let e = 
    match e with
    | BEXPR_uniq e,_ -> e
    | _ -> e
  in
  Flx_bexpr.map ~f_btype ~f_bexpr e 

let fixexpr bsym_table x = 
  let y = fixexpr' bsym_table x in
(* print_endline ("  %%%%% Fixexpr " ^ sbe bsym_table x ^ " --> " ^ sbe bsym_table y); *)
  y

let fixbexe bsym_table x =
  let y = Flx_bexe.map ~f_btype:(fixtype bsym_table) ~f_bexpr:(fixexpr bsym_table) x in
(* print_endline ("    &&&&&& Fixbexe " ^ string_of_bexe bsym_table 4 x ^ " ----> " ^ string_of_bexe bsym_table 4 y); *)
  y

let fixbexes bsym_table bexes = 
  let unit_t = Flx_btype.btyp_unit () in
  let fbx x = fixbexe bsym_table x in
  let pr lst x = match fbx x with
  | Flx_bexe.BEXE_assign (sr,(a,at),(_,t)) when t = unit_t -> assert (t=at); lst
  | Flx_bexe.BEXE_init (sr,_,(_,t)) when t = unit_t -> lst
  | y -> y::lst
  in
  List.rev (
    List.fold_left pr [] bexes
  )

let fixps bsym_table ps = 
  Flx_bparams.map ~f_btype:(fixtype bsym_table) ~f_bexpr:(fixexpr bsym_table) ps

let fix_symbol bsym_table index parent bsym bsym_table' =
  let ft t = fixtype bsym_table t in
  let fts ts = map (fixtype bsym_table) ts in
  let fe e = fixexpr bsym_table e in
  let fxs xs = fixbexes bsym_table xs in
  let fp bps = fixps bsym_table bps in
  let fb rqs = fixreqs bsym_table rqs in
  let fq qs = fixquals bsym_table qs in

  let h bbdcl = Flx_bsym_table.add bsym_table' index parent { bsym with Flx_bsym.bbdcl= bbdcl } in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_nominal_type_alias _
  | BBDCL_structural_type_alias _
  | BBDCL_virtual_type _
  | BBDCL_instance_type _
  | BBDCL_invalid
  | BBDCL_module
  | BBDCL_typeclass _
  | BBDCL_instance _
  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce 
    -> assert false

  | BBDCL_newtype (bvs, t) -> ()

  | BBDCL_label s as x -> h x

  | BBDCL_fun (props, bvs, bps, ret, effects, bexes) ->
      h (bbdcl_fun (props, bvs, fp bps, ft ret, ft effects, fxs bexes))

  | BBDCL_val (bvs, t, kind) ->
      h (bbdcl_val (bvs, ft t, kind))

  | BBDCL_external_type (bvs, btqs, c, breqs) ->
      h (bbdcl_external_type (bvs, fq btqs, c, fb breqs))

  | BBDCL_external_const (props, bvs, t, c, breqs) ->
      h (bbdcl_external_const (props,  bvs, ft t, c, fb breqs))

  | BBDCL_external_fun (props, bvs, ts, t, breqs, prec, kind) ->
      (* Ignore identity functions. *)
      if kind = `Code Flx_code_spec.Identity then () else
      let kind =
        match kind with
        | `Callback (ts2, j) -> `Callback (fts ts2, j)
        | _ -> kind
      in
      h (bbdcl_external_fun (props, bvs, fts ts, ft t, fb breqs, prec, kind))

  | BBDCL_external_code (bvs, c, ikind, breqs) ->
      h (bbdcl_external_code (bvs, c, ikind, fb breqs))

(*
  (* eliminate unions with solo constructors *)
  | BBDCL_union (bvs, [id,idx,ct]) -> 
print_endline ("Removing entry for index = "^si index^" : union " ^Flx_bsym.id bsym ^ " with solo constructor " ^ id);
    ()
*)

  | BBDCL_union (bvs, cts) ->
      let cts = map (fun (s,j,evs,d,c,gadt) -> s,j,evs,ft d, ft c,gadt) cts in
      h (bbdcl_union (bvs, cts))

  | BBDCL_struct (bvs, cts) ->
      let cts = map (fun (s,t) -> s,ft t) cts in
      h (bbdcl_struct (bvs, cts))

  | BBDCL_cstruct (bvs, cts, breqs) ->
      let cts = map (fun (s,t) -> s,ft t) cts in
      h (bbdcl_cstruct (bvs, cts, fb breqs))
(*  
  | BBDCL_const_ctor (bvs, j, t1, k, evs, etraint) when is_solo_union bsym_table t1 -> 
print_endline ("Removing entry for index = "^si index^
" : const constructor index "^si k^ " name " ^ Flx_bsym.id bsym ^
" for union " ^sbt bsym_table t1 ^ " (solo constructor)");
    ()

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) when is_solo_union bsym_table t1 ->
print_endline ("Removing entry for index = "^si index^
" : nonconst constructor index "^si k^ " name " ^ Flx_bsym.id bsym ^
" arg type " ^ sbt bsym_table t2 ^
" for union " ^sbt bsym_table t1 ^ " (solo constructor)");
    ()
*)

  | BBDCL_const_ctor (bvs, j, t1, k, evs, etraint) ->
      h (bbdcl_const_ctor (bvs, j, ft t1, k, evs, ft etraint))

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) ->
      h (bbdcl_nonconst_ctor (bvs, j, ft t1, k, ft t2, evs, ft etraint))

let struniq bsym_table =
  let bsym_table' : Flx_bsym_table.t = Flx_bsym_table.create_from bsym_table in
  Flx_bsym_table.iter
    (fun bid parent sym -> 
      try
        fix_symbol bsym_table bid parent sym bsym_table'
      with Not_found ->
        failwith ("Struniq chucked not found processing " ^ string_of_int bid)
    )
    bsym_table
  ;
  bsym_table'




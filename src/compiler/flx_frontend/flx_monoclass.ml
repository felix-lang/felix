open Flx_bexpr
open Flx_bexe

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING REFS TO VIRTUALS WITH INSTANCES      *)
(* ----------------------------------------------------------- *)

(* these routine use an instantiation function that replaces
references to virtual functions to references to instance functions.
The reference indices should be a ts list of monomorphic types,
in which case we get back a reference to a instance, also
with monomorphic types.

*)

 
(* this one should not do anything useful now *)
let flat_typeclass_fixup_type syms bsym_table virtualinst sr t =
  match t with
  | Flx_btype.BTYP_inst (i,ts,mt) ->
    let i',ts' = virtualinst sr i ts in
    let t = Flx_btype.btyp_inst (i',ts',mt) in
    t
  | x -> x

(* bottom up mapping *)
let rec typeclass_fixup_type syms bsym_table virtualinst sr t =
  let f_btype t = typeclass_fixup_type syms bsym_table virtualinst sr t in
  let t = Flx_btype.map ~f_btype t in
  let t = flat_typeclass_fixup_type syms bsym_table virtualinst sr t in
  t

let flat_typeclass_fixup_expr syms bsym_table virtualinst sr (e,t) =
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) -> assert false
  | BEXPR_apply_direct (i,ts,a) -> assert false
  | BEXPR_apply_struct (i,ts,a) -> assert false
  | BEXPR_apply_stack (i,ts,a) -> assert false
  | BEXPR_ref (i,ts)  ->
    let i,ts = virtualinst sr i ts in
    bexpr_ref t (i,ts)

  | BEXPR_varname (i',ts') ->
    let i,ts = virtualinst sr i' ts' in
    bexpr_varname t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = virtualinst sr i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  x

(* bottom up mapping *)
let rec typeclass_fixup_expr syms bsym_table virtualinst sr e =
  let f_bexpr e = typeclass_fixup_expr  syms bsym_table virtualinst sr e in
  let e = flat_typeclass_fixup_expr syms bsym_table virtualinst sr e in
  let e = Flx_bexpr.map ~f_bexpr e in
  e 

(* mt is only used to fixup svc and init hacks *)
let flat_typeclass_fixup_exe syms bsym_table polyinst mt exe =
(*
  print_endline ("TYPECLASS FIXUP EXE[In] =" ^ string_of_bexe bsym_table 0 exe);
*)
  let result =
  match exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
  | BEXE_jump_direct (sr, i,ts,a) -> assert false
  | BEXE_call_prim (sr, i',ts,a) -> assert false
  | BEXE_call_stack (sr, i,ts,a) -> assert false

  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result



let sbe x = Flx_print.sbe x
let show_exe bsym_table exe = Flx_print.string_of_bexe bsym_table 4 exe
let show_exes bsym_table exes = Flx_util.catmap "\n" (show_exe bsym_table) exes


(* ----------------------------------------------------------- *)
(* COMPLETE PROCESSING ROUTINES                                *)
(* ----------------------------------------------------------- *)

(* NOTE: THE NASTY ENTANGLEMENT *)

(* These routine produce results which are designed for the NEW monomorphic
  symbol table, so all the indices are for that table not the old one.
*)

let fixup_type syms bsym_table vars bsym virtualinst polyinst sr t =
(*
  print_endline ("    ** mono_type " ^ sbt bsym_table t);
*)
  let t = Flx_monosubs.mono_type syms bsym_table vars sr t in
(*
  print_endline ("    ** typeclass_fixup_type " ^ sbt bsym_table t);
*)
  let t = Flx_monoclass.typeclass_fixup_type syms bsym_table virtualinst sr  t in
(*
  print_endline ("    ** Betareduce " ^ sbt bsym_table t);
*)
  let t = Flx_beta.beta_reduce "flx_mono: mono, metatype"
    syms.Flx_mtypes2.counter
    bsym_table
    (Flx_bsym.sr bsym)
    t
  in 
(*
  print_endline ("    ** poly_fixup_type " ^ sbt bsym_table t);
*)
  let t = Flx_polyinst.poly_fixup_type syms bsym_table polyinst sr t in
(*
  print_endline ("    ** Polyfixedup" ^ sbt bsym_table t );
*)
  t

let fixup_req syms bsym_table vars polyinst sr (i,ts) : Flx_bid.bid_t * Flx_btype.t list =
  let ts = List.map (Flx_monosubs.mono_type syms bsym_table vars sr) ts in
  let j,ts = polyinst sr i ts in
  let ts = List.map (Flx_polyinst.poly_fixup_type syms bsym_table polyinst sr) ts in
  j,ts

let fixup_reqs syms bsym_table vars polyinst sr reqs : Flx_btype.breqs_t = 
  List.map (fixup_req syms bsym_table vars polyinst sr) reqs

(* HUH? Never called??? Oh, yes, used in constraints .. *) 
let fixup_expr syms bsym_table monotype virtualinst polyinst sr e =
  print_endline ("[fixup_expr] input               : " ^ sbe bsym_table e);
  (* monomorphise the code by eliminating type variables *)
  let e = Flx_bexpr.map ~f_btype:monotype e in
(*
  print_endline ("[fixup_expr] monomorphised       : " ^ sbe bsym_table e);
*)
  (* eliminate virtual calls by mapping to instances *)
  let e = Flx_monoclass.typeclass_fixup_expr syms bsym_table virtualinst sr e in
(*
  print_endline ("[fixup_expr] virtuals eliminated : " ^ sbe bsym_table e);
*)
  (* replace applications of polymorphic function (or variable)
    with applications of new monomorphic ones
  *)
  let e = Flx_polyinst.poly_fixup_expr syms bsym_table polyinst sr e in
(*
  print_endline ("[fixup_expr] polysyms eliminated : " ^ sbe bsym_table e);
*)
  e

(* completely process a list of exes *)
(* rewrite to do in one pass *)
let fixup_exes syms bsym_table vars virtualinst polyinst parent_ts exes =
 let mt t = Flx_monosubs.mono_type syms bsym_table vars t in

 (* monomorphise the code by eliminating type variables *)
(*
  print_endline ("To fixup exes:\n" ^ show_exes bsym_table exes);
*)
  let rexes = List.fold_left 
    (fun oexes iexe -> 
      match iexe with
      | Flx_bexe.BEXE_call (sr,(Flx_bexpr.BEXPR_closure (f,[]),_),_) ->
        begin match Flx_bsym_table.find_bbdcl bsym_table f with
        (* elide calls to empty non-virtual procedures 
           Inlining does this anyhow, but this cleans up the
           diagnostic prints and reduces the crap in the symbol
           table a little earlier.
        *) 
        | Flx_bbdcl.BBDCL_fun (props,_,_,_,_,[]) when not (List.mem `Virtual props) -> oexes 
        | _ -> Flx_monosubs.mono_exe syms bsym_table vars iexe :: oexes
        end 
      | _ ->  Flx_monosubs.mono_exe syms bsym_table vars iexe :: oexes
    ) 
    [] 
    exes 
  in
(*
  print_endline ("Monomorphised:\n" ^ show_exes bsym_table (List.rev rexes));
  print_endline ("VARS=" ^ showvars bsym_table vars);
*)
  (* eliminate virtual calls by mapping to instances *)
  (* order doesn't matter here *)
  let exes = List.rev_map 
    (fun exe -> 
      let sr = Flx_bexe.get_srcref exe in 
      Flx_bexe.map ~f_bexpr:(Flx_monoclass.typeclass_fixup_expr syms bsym_table virtualinst sr) 
      exe
    ) 
    rexes 
  in
  let rexes = List.rev_map (fun exe -> Flx_monoclass.flat_typeclass_fixup_exe syms bsym_table virtualinst mt exe) exes in
(*
  print_endline ("Virtuals Instantiated:\n" ^ show_exes bsym_table (List.rev exes));
*)
  let exes = List.rev_map (Flx_polyinst.flat_poly_fixup_exe syms bsym_table polyinst parent_ts mt)  rexes in
(*
  print_endline ("Special calls monomorphised:\n" ^ show_exes bsym_table exes);
*)
  (* replace applications of polymorphic function (or variable)
    with applications of new monomorphic ones
  *)
  let exes = List.map 
    (
      fun exe -> let sr = Flx_bexe.get_srcref exe in
      Flx_bexe.map ~f_bexpr:(Flx_polyinst.poly_fixup_expr syms bsym_table polyinst sr) 
      exe
    ) 
    exes 
  in
(*
  print_endline ("Applies polyinst:\n" ^ show_exes bsym_table exes);
*)
  exes

let fixup_qual vars mt qual = 
  match qual with
  | `Bound_needs_shape t -> `Bound_needs_shape (mt vars t)
  | x -> x
 


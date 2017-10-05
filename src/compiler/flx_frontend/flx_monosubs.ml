(* These functions take a term and perform type substitutions 
  using the provided substitution map. 

  If every type variable used in the term is in the map,
  and, every substitution in the map is monomorphic,
  the resultant term will be monomorphic.

  The substitution map is an association list of indices and types.

*)


(* This one is nasty, because it has to beta reduce after substitution,
and then, it has to actually replace virtual types we monomorphic
ones as well. Normally virtual to instance mapping would not occur
here, but it really is a substitution for a type variable,
its just the the type variable is a virtual, a different kind
of type variable (in fact, existential).

FIXME: Its NOT clear to me why the result of that operation has to actually
be monomorphic. This is probably a bug.  However there's a check in there
that it is and I won't fix it until I have an example that triggers
the assertion.

*)

let mono_type syms bsym_table vars sr t = 
(*
print_endline (" ** begin mono_type " ^ sbt bsym_table t);
*)
  let t = Flx_btype_subst.list_subst syms.Flx_mtypes2.counter vars t in
(*
print_endline (" ** mono_type after variable replacement " ^ sbt bsym_table t);
*)
  let t = Flx_beta.beta_reduce "mono_type"
    syms.Flx_mtypes2.counter
    bsym_table
    Flx_srcref.dummy_sr
    t
  in 
  let t = Flx_build_tctab.remap_virtual_types syms bsym_table (* tc *) t in
  begin try Flx_monocheck.check_mono bsym_table sr t with _ -> assert false end;
(*
print_endline (" ** end Mono_type " ^ sbt bsym_table t);
*)
  t

let rec mono_expr syms bsym_table vars sr e =
(*
print_endline (" ** begin mono_expr " ^ sbe bsym_table e);
print_endline (" ** begin mono_expr: type " ^ sbt bsym_table (snd e));
*)
  let f_btype t = mono_type syms bsym_table vars sr t in
  let f_bexpr e = mono_expr syms bsym_table vars sr e in
  let e = Flx_bexpr.map ~f_btype ~f_bexpr e in
(*
print_endline (" ** end mono_expr " ^ sbe bsym_table e);
print_endline (" ** end mono_expr: type " ^ sbt bsym_table (snd e));
*)
  e

let rec mono_exe syms bsym_table vars exe =
(*
print_endline (" ** begin mono_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  let sr = Flx_bexe.get_srcref exe in
  let f_btype t = mono_type syms bsym_table vars sr t in
  let f_bexpr e = mono_expr syms bsym_table vars sr e in
  let exe = Flx_bexe.map ~f_btype ~f_bexpr exe in
(*
print_endline (" ** end mono_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  exe



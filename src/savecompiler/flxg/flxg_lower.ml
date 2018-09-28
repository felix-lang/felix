open Flxg_state


(** Lower the high level constructs into simpler ones. *)
let lower state bsym_table root_proc =
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//LOWERING";

  let bsym_table = Flx_lower.lower_bsym_table
    (Flx_lower.make_lower_state state.syms)
    bsym_table
    root_proc
  in

  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//LOWERING OK";

  bsym_table


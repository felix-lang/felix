open Flxg_state


(** Optimize the bound symbols. *)
let optimize state bsym_table root_proc =
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//OPTIMIZING";

  let bsym_table = Flx_opt.optimize_bsym_table
    state.syms
    bsym_table
    root_proc
  in

  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//OPTIMIZATION OK";

  bsym_table

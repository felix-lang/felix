open Format

open Flxg_state


(** Optimize the bound symbols. *)
let optimize state bsym_table root_proc =
  fprintf state.ppf "//OPTIMIZING\n";

  let bsym_table = Flx_opt.optimize_bsym_table
    state.syms
    bsym_table
    root_proc
  in

  fprintf state.ppf "//OPTIMIZATION OK\n";

  bsym_table

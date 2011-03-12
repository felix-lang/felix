open Format

open Flxg_state


(** Optimize the bound symbols. *)
let optimize state bsym_table root_proc =
  fprintf state.ppf "//OPTIMIZING\n";
  let opt_timer = Flxg_profile.make_timer () in

  let bsym_table = Flx_opt.optimize_bsym_table
    state.syms
    bsym_table
    root_proc
  in

  state.opt_time <- state.opt_time +. opt_timer ();
  fprintf state.ppf "//OPTIMIZATION OK time %f\n" state.opt_time;

  bsym_table

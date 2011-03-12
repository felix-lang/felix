open Format

open Flxg_state


(** Lower the high level constructs into simpler ones. *)
let lower state bsym_table root_proc =
  fprintf state.ppf "//LOWERING\n";
  let lower_timer = Flxg_profile.make_timer () in

  let bsym_table = Flx_lower.lower_bsym_table
    (Flx_lower.make_lower_state state.syms)
    bsym_table
    root_proc
  in

  state.lower_time <- state.lower_time +. lower_timer ();
  fprintf state.ppf "//LOWERING OK time %f\n"state.lower_time;

  bsym_table

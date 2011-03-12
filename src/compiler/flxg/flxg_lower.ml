open Format

open Flxg_state


(** Lower the high level constructs into simpler ones. *)
let lower state bsym_table root_proc =
  fprintf state.ppf "//LOWERING\n";

  let bsym_table = Flx_lower.lower_bsym_table
    (Flx_lower.make_lower_state state.syms)
    bsym_table
    root_proc
  in

  fprintf state.ppf "//LOWERING OK\n";

  bsym_table

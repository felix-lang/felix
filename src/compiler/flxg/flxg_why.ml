open Flxg_state


(** Generate the why file. *)
let generate_why_file state bsym_table root_proc =
  (* generate why file *)
  Flx_why.emit_whycode
    (Flxg_file.filename state.why_file)
    state.syms
    bsym_table
    root_proc

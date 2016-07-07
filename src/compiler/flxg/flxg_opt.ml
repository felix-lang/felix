open Flxg_state

(* -------------------------------------------------------------------------- *)
(** Optimize the bound symbols. *)
let optimize state bsym_table (root_proc:int option) =

  (* Debugging. *)
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
    print_endline "//OPTIMIZING";

  (* Skip reductions if the compiler flag is set. *)
  if not state.syms.Flx_mtypes2.compiler_options.Flx_options.doreductions then
     state.syms.Flx_mtypes2.reductions := []; 

  (* Do the work *)
  let bsym_table = Flx_opt.optimize_bsym_table
    state.syms
    bsym_table
    root_proc
  in

  (* Debugging. *)
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
    print_endline "//OPTIMIZATION OK";

  bsym_table


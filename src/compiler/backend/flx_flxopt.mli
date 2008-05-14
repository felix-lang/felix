open Flx_mtypes2

val get_felix_options:
  (string * string) list ->
  felix_compiler_options_t

val make_syms:
  felix_compiler_options_t -> sym_state_t

val print_options:
  unit -> unit

val print_chosen:
  (string * string) list ->
  unit

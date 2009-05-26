val get_felix_options:
  (string * string) list ->
  Flx_mtypes2.felix_compiler_options_t

val print_options:
  unit -> unit

val print_chosen:
  (string * string) list ->
  unit

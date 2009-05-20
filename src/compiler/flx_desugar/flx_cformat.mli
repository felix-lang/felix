(** C format string *)

val types_of_cformat_string:
  Flx_srcref.t ->
  string ->
  string * (int * Flx_ast.typecode_t) list

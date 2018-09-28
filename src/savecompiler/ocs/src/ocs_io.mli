(* I/O primitives *)

open Ocs_types

val read : thread -> (sval -> unit) -> sval array -> unit

val open_input_file : sval -> sval
val open_output_file : sval -> sval

val init : env -> unit


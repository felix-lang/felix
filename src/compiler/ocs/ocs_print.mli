(* Print Scheme values *)

open Ocs_types

val write_string : Ocs_port.port -> string -> unit
val write_char : Ocs_port.port -> char -> unit

val print : Ocs_port.port -> bool -> sval -> unit


(* Print Scheme values *)

open Ocs_types

val store_string : (char->unit) -> (bytes->unit) -> bytes-> unit
val store_char : (char->unit) -> (bytes->unit) -> char -> unit
val store : (char->unit) -> (bytes->unit) -> bool -> sval -> unit

val write_string : Ocs_port.port -> bytes -> unit
val write_char : Ocs_port.port -> char -> unit

val print : Ocs_port.port -> bool -> sval -> unit

val write_string_to_buffer : Buffer.t -> bytes -> unit
val write_char_to_buffer : Buffer.t -> char -> unit
val print_to_buffer : Buffer.t -> bool -> sval -> unit

val string_of_ocs: sval -> bytes 

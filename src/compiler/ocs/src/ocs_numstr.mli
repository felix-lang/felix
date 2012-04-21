(* Conversions between numbers and strings.  *)

open Ocs_types

val string_of_real : float -> string
val string_of_complex : Complex.t -> string

val string_to_num : string -> int -> sval

val init : env -> unit


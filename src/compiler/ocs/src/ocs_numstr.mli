(* Conversions between numbers and strings.  *)

open Ocs_types

val string_of_real : float -> bytes
val string_of_complex : Complex.t -> bytes

val string_to_num : bytes -> int -> sval

val init : env -> unit


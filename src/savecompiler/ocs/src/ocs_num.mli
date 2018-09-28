(* Operations on number types.  *)

open Ocs_types

val negate : sval -> sval

val add2 : sval -> sval -> sval
val sub2 : sval -> sval -> sval
val mul2 : sval -> sval -> sval
val div2 : sval -> sval -> sval

val init : env -> unit


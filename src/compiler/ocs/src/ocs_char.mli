(* Character primitives.  *)

open Ocs_types

val name_to_char : bytes -> char option
val char_to_name : char -> bytes

val init : env -> unit


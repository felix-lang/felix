(** C format string *)

open Flx_srcref
open Flx_ast

val types_of_cformat_string:
  range_srcref ->
  string ->
  string * (int * typecode_t) list

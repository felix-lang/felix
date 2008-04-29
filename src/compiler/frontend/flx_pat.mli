(** Pattern matching utilities *)

open Flx_ast

val validate_patterns:
  pattern_t list -> unit

val is_universal:
  pattern_t -> bool

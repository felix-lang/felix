(** Pattern matching utilities *)

open Flx_ast

val validate_patterns:
  pattern_t list -> unit

val is_irrefutable:
  pattern_t -> bool

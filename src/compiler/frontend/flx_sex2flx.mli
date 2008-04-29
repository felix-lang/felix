(** Extension experiment *)

open Sex_types
open Flx_ast
open Flx_types

exception Sex2FlxTypeError of string * sexp_t

val xstatement_t:
  range_srcref ->
  sexp_t ->
  statement_t

val xexpr_t:
  range_srcref ->
  sexp_t ->
  expr_t

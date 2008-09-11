(** Lexer generator *)

open Flx_ast
open Flx_types

type reg_kind_t =
[
  | `regmatch of string * string
  | `reglex of string * string * string
]

val regen:
  Buffer.t ->
  range_srcref ->
  regular_args_t ->
  reg_kind_t ->
  (tbexpr_t -> string) ->
  unit

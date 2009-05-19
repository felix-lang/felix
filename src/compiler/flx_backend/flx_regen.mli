(** Lexer generator *)

type reg_kind_t =
[
  | `regmatch of string * string
  | `reglex of string * string * string
]

val regen:
  Buffer.t ->
  Flx_srcref.t ->
  Flx_types.regular_args_t ->
  reg_kind_t ->
  (Flx_types.tbexpr_t -> string) ->
  unit

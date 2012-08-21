(** Extension experiment *)

exception Sex2FlxTypeError of string * Sex_types.sexp_t

val xstatement_t:
  Flx_srcref.t ->
  Sex_types.sexp_t ->
  Flx_ast.statement_t

val xexpr_t:
  Flx_srcref.t ->
  Sex_types.sexp_t ->
  Flx_ast.expr_t

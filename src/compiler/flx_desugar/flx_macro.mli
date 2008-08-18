(** Macro expansion
 *
 * Felix macros have 3 flavours:
 *
 * {[
 * keyword | kind      | subst
 * --------+-----------+-------------
 * ident   | name      | exe expr+dcl
 * fun     | expr      | exe expr
 * proc    | statement | exe stmt
 * ]}
 *
 * Name macros allow renaming of functions, procedures, values, consts,
 * variables, the body must be an identifier or the keyword new.  Expression
 * macros replace applications in expressions.  Statement macros replace call
 * statement.
 *
 * Macros are expanded by evaluating the arguments if any, substituting the
 * arguments into the body, and then evaluating the body.
 *
 * Evaluation is by recursive descent with rescanning.
 *
 * Note: name macros replace names in executable code, including macro bodies,
 * but they cannot be used to rename macros. *)

open Flx_ast
val expand_macros:
  string ->
  int ->
  statement_t list ->
  statement_t list

(** [expand_expr] is a special hook used to perform
  constant folding and desugaring in the preprocessor
*)
val expand_expression:
  string -> expr_t -> expr_t

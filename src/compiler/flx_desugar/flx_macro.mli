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


type macro_t =
 | MVal of Flx_ast.expr_t
 | MName of Flx_id.t

type macro_dfn_t = Flx_id.t * macro_t

type macro_state_t

val make_macro_state:
  ?recursion_limit:int -> (** How deep to recurse. *)
  string ->               (** Local prefix used for creating unique names. *)
  int ref ->
  macro_state_t

(** Expand all the macros in the statements. *)
val expand_macros:
  macro_state_t ->            (** Macro state. *)
  Flx_ast.statement_t list -> (** Statements to expand. *)
  Flx_ast.statement_t list

val get_macro_seq: macro_state_t -> int

val expand_expr: 
  int -> string -> int ref -> 
  macro_dfn_t list -> Flx_ast.expr_t -> Flx_ast.expr_t 


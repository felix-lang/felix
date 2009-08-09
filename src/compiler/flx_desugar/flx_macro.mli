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

type macro_state_t

val make_macro_state:
  ?recursion_limit:int -> (** How deep to recurse. *)
  string ->               (** Local prefix used for creating unique names. *)
  macro_state_t

(** Expand all the macros in the statements. *)
val expand_macros:
  macro_state_t ->            (** Macro state. *)
  Flx_ast.statement_t list -> (** Statements to expand. *)
  Flx_ast.statement_t list

(** Expand all the macros in the statement. *)
val expand_macros_in_statement:
  macro_state_t ->                      (** Macro state. *)
  ('a -> Flx_ast.statement_t -> 'a) ->  (** Fold this over each statement *)
  'a ->                                 (** The initial value. *)
  Flx_ast.statement_t ->                (** Statement to expand. *)
  'a
